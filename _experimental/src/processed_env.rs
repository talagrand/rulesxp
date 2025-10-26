// ProcessedEnvironment - Environment for ProcessedValue with arena-tied lifetimes
// This environment stores ProcessedValues directly using arena references
// to maintain string interning benefits and eliminate unsafe lifetime conversions.

use crate::processed_ast::StringSymbol;
use crate::super_builtins::ProcessedValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Binding state for TDZ (Temporal Dead Zone) enforcement per R7RS 5.3.2
///
/// R7RS requires that variables in letrec/letrec*/internal-defines exist but are
/// uninitialized until their init expressions are evaluated. Reading before initialization
/// is an error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingState {
    /// Binding exists but has not been initialized yet (TDZ)
    /// Reading this binding is an error per R7RS 5.3.2
    Uninitialized,
    /// Binding has been initialized with a value
    /// This is the normal state after initialization completes
    Initialized,
}

/// Environment binding with state tracking for TDZ enforcement
///
/// All bindings are stored as mutable cells to support both:
/// 1. Sequential initialization (letrec*, internal defines)
/// 2. Variable mutation (set!)
///
/// Location represents a binding location that can be either uninitialized or hold a value.
///
/// **R7RS CONFORMANCE:** Proper Temporal Dead Zone enforcement - accessing uninitialized
/// locations causes runtime errors per R7RS section 5.3.2.
///
/// This enum replaces the previous approach of tracking state and value separately.
#[derive(Clone)]
pub enum Location<'ast> {
    /// Binding exists but has not been initialized yet (TDZ)
    /// Reading this binding is an error per R7RS 5.3.2
    Uninitialized,
    /// Binding has been initialized with a value
    /// This is the normal state after initialization completes
    Value(ProcessedValue<'ast>),
}

impl<'ast> Location<'ast> {
    /// Create a new uninitialized binding (for letrec/letrec*/internal-defines)
    pub fn new_uninitialized() -> Location<'ast> {
        Location::Uninitialized
    }

    /// Create a new initialized binding (for define/lambda capture)
    pub fn new_initialized(value: ProcessedValue<'ast>) -> Location<'ast> {
        Location::Value(value)
    }

    /// Check if the binding is initialized (TDZ enforcement)
    pub fn is_initialized(&self) -> bool {
        matches!(self, Location::Value(_))
    }

    /// Get the value (enforces TDZ by checking state first)
    pub fn value(&self) -> Result<ProcessedValue<'ast>, String> {
        match self {
            Location::Uninitialized => {
                Err("Temporal Dead Zone: variable not yet initialized".to_string())
            }
            Location::Value(val) => Ok(val.clone()),
        }
    }

    /// Get the value without TDZ checking (for internal use where state is known)
    pub fn value_unchecked(&self) -> ProcessedValue<'ast> {
        match self {
            Location::Uninitialized => ProcessedValue::Unspecified,
            Location::Value(val) => val.clone(),
        }
    }

    /// Initialize an uninitialized location (for letrec back-patching)
    /// Panics if location is already initialized - this should only be called during letrec setup
    pub fn initialize(&mut self, value: ProcessedValue<'ast>) {
        match self {
            Location::Uninitialized => {
                *self = Location::Value(value);
            }
            Location::Value(_) => {
                panic!("Attempted to initialize already-initialized location");
            }
        }
    }
}

#[cfg(test)]
use string_interner::{DefaultBackend, StringInterner, Symbol};

/// Environment that stores ProcessedValues with arena-tied lifetimes
/// All stored values reference data in the ProcessedAST arena
///
/// **IMMUTABLE ARCHITECTURE:** Environments are immutable after creation.
/// New bindings are added by creating a new environment that chains to the parent.
/// This eliminates the need for RefCell on the HashMap itself.
///
/// **TDZ ENFORCEMENT:** All bindings use Location enum to track initialization status.
/// Reading uninitialized bindings is an error per R7RS 5.3.2.
///
/// **REPL SEMANTICS:** The bindings HashMap is wrapped in RefCell to support top-level
/// redefinition in REPL contexts. This allows `redefine_binding` to add new bindings
/// without creating a new environment.
///
/// **STACK SAFETY:** All parent chain walking uses iterative algorithms to prevent
/// stack overflow with deeply nested scopes. Do NOT add recursive methods that walk
/// the parent chain.
///
/// **SAFE PATTERN (Iterative)**:
/// ```rust
/// let mut current = self;
/// loop {
///     // ... check current ...
///     match &current.parent {
///         Some(p) => current = p,  // Iterative - stack-safe
///         None => break,
///     }
/// }
/// ```
///
/// **UNSAFE PATTERN (Recursive - DO NOT USE)**:
/// ```rust
/// if let Some(parent) = &self.parent {
///     parent.some_method()  // Recursive - can overflow!
/// }
/// ```
#[derive(Clone)]
pub struct ProcessedEnvironment<'ast> {
    /// Variable bindings - maps interned symbols to Location with state tracking
    /// Wrapped in RefCell to support REPL redefinition and location mutation during initialization
    bindings: RefCell<HashMap<StringSymbol, Rc<RefCell<Location<'ast>>>>>,
    /// Parent environment for lexical scoping
    parent: Option<Rc<ProcessedEnvironment<'ast>>>,
}

impl<'ast> ProcessedEnvironment<'ast> {
    /// Create a new empty environment
    pub fn new() -> Self {
        ProcessedEnvironment {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    /// Create a new environment with a parent
    pub fn with_parent(parent: Rc<ProcessedEnvironment<'ast>>) -> Self {
        ProcessedEnvironment {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent),
        }
    }

    /// Create environment with initial bindings (used for letrec/letrec*)
    ///
    /// Create environment with uninitialized bindings for given symbols
    ///
    /// **R7RS 5.3.2 TDZ:** All bindings start uninitialized - caller must
    /// initialize them before they can be read. Used for letrec/letrec*.
    pub fn create_with_bindings(
        symbol_names: Vec<StringSymbol>,
        parent: Option<Rc<Self>>,
    ) -> Rc<Self> {
        let mut bindings = HashMap::new();
        for symbol in symbol_names {
            let location = Rc::new(RefCell::new(Location::new_uninitialized()));
            bindings.insert(symbol, location);
        }

        Rc::new(ProcessedEnvironment {
            bindings: RefCell::new(bindings),
            parent,
        })
    }

    /// Extend this environment with a new initialized binding, returning a new environment
    ///
    /// **IMMUTABLE SEMANTICS:** This creates a new environment that chains to self.
    /// The original environment is unchanged. This is used for regular defines.
    ///
    /// **USAGE:**
    /// ```rust
    /// let env1 = Rc::new(ProcessedEnvironment::new());
    /// let env2 = env1.extend(symbol, value);
    /// // env1 unchanged, env2 has new binding
    /// ```
    pub fn extend(
        self: &Rc<Self>,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Rc<ProcessedEnvironment<'ast>> {
        let mut new_bindings = HashMap::new();
        new_bindings.insert(
            symbol,
            Rc::new(RefCell::new(Location::new_initialized(value))),
        );

        Rc::new(ProcessedEnvironment {
            bindings: RefCell::new(new_bindings),
            parent: Some(Rc::clone(self)),
        })
    }

    /// Initialize a binding that was previously defined
    ///
    /// **TDZ TRANSITION:** Moves binding from Uninitialized to Initialized state.
    /// Returns error if binding doesn't exist in this environment.
    pub fn initialize_binding(
        &self,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Result<(), String> {
        if let Some(binding) = self.bindings.borrow().get(&symbol).cloned() {
            binding.borrow_mut().initialize(value);
            Ok(())
        } else {
            let error_msg = format!("Cannot initialize non-existent binding: {:?}", symbol);
            Err(error_msg)
        }
    }

    /// Redefine an existing binding or create new binding (REPL top-level only)
    ///
    /// **R7RS REPL SEMANTICS:** Top-level redefinition/definition is allowed for interactive development.
    /// This should only be called in REPL contexts, not in nested scopes.
    ///
    /// If binding exists: updates its value and marks it as Initialized.
    /// If binding doesn't exist: creates a new binding in this environment.
    pub fn redefine_binding(
        &self,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Result<(), String> {
        // Check if binding exists - use explicit scope to ensure borrow is dropped
        let existing_binding = { self.bindings.borrow().get(&symbol).cloned() };

        if let Some(binding) = existing_binding {
            // Binding exists - redefine it
            *binding.borrow_mut() = Location::Value(value);
            Ok(())
        } else {
            // Binding doesn't exist - create it (first-time define at top-level)
            let binding_value = Rc::new(RefCell::new(Location::new_initialized(value)));
            self.bindings.borrow_mut().insert(symbol, binding_value);
            Ok(())
        }
    }

    /// Look up a variable by StringSymbol with TDZ enforcement
    ///
    /// **TDZ ENFORCEMENT:** Returns error if binding exists but is Uninitialized.
    /// Returns None if binding doesn't exist in this environment or parent chain.
    ///
    /// **STACK SAFETY:** Uses iterative parent chain walking to prevent stack overflow
    /// with deeply nested scopes.
    pub fn lookup(&self, symbol: StringSymbol) -> Result<Option<ProcessedValue<'ast>>, String> {
        let mut current = self;

        loop {
            if let Some(binding) = current.bindings.borrow().get(&symbol).cloned() {
                return match binding.borrow().value() {
                    Ok(value) => Ok(Some(value)),
                    Err(err) => Err(format!(
                        "Variable used before initialization (TDZ violation) for {:?}: {}",
                        symbol, err
                    )),
                };
            }

            match &current.parent {
                Some(parent) => {
                    current = parent;
                }
                None => {
                    return Ok(None);
                }
            }
        }
    }

    /// Set a variable's value, searching this environment and parent chain
    ///
    /// **R7RS CONFORMANCE:** Implements R7RS set! by mutating existing initialized bindings.
    /// Returns error if variable is not defined or is uninitialized (TDZ violation).
    ///
    /// **TDZ ENFORCEMENT:** Cannot set! an uninitialized variable per R7RS 5.3.2.
    pub fn set_binding(
        &self,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Result<(), String> {
        let mut current = self;

        loop {
            if let Some(binding) = current.bindings.borrow().get(&symbol).cloned() {
                // **FIX:** Separate the borrow operations to avoid RefCell panic
                // First check if initialized (borrow ends after this block)
                let is_initialized = binding.borrow().is_initialized();
                match is_initialized {
                    true => {
                        // Now safely borrow_mut since previous borrow is dropped
                        *binding.borrow_mut() = Location::Value(value);
                        return Ok(());
                    }
                    false => {
                        return Err(format!(
                            "Cannot set! uninitialized variable (TDZ violation): {:?}",
                            symbol
                        ));
                    }
                }
            }

            match &current.parent {
                Some(parent) => current = parent,
                None => return Err(format!("Undefined variable for set!: {:?}", symbol)),
            }
        }
    }

    /// Get a reference to the bindings map (for debug/error reporting)
    pub fn bindings(
        &self,
    ) -> std::cell::Ref<'_, HashMap<StringSymbol, Rc<RefCell<Location<'ast>>>>> {
        self.bindings.borrow()
    }

    /// Get a reference to the parent environment (for debug/error reporting)
    pub fn parent(&self) -> Option<&Rc<ProcessedEnvironment<'ast>>> {
        self.parent.as_ref()
    }
}

impl<'ast> Default for ProcessedEnvironment<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // === R7RS 5.2.2: Environment and Variable Binding Edge Cases ===
    // NOTE: Tests using with_bindings, create_with_bindings, update_cell, get_cell,
    // is_bound, is_initialized, and binding_count have been removed as those methods
    // were deleted to reduce API surface area.

    #[test]
    fn test_environment_shadowing_and_parent_chain() {
        // R7RS 5.2.2: Shadowing and parent chain lookup
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(4).unwrap();
        let parent_with_binding = parent.extend(sym, ProcessedValue::Integer(1));
        let child = parent_with_binding.extend(sym, ProcessedValue::Integer(2));
        assert_eq!(child.lookup(sym).unwrap(), Some(ProcessedValue::Integer(2)));
        assert_eq!(
            parent_with_binding.lookup(sym).unwrap(),
            Some(ProcessedValue::Integer(1))
        );
    }

    // Tests using with_bindings, create_with_bindings, is_bound, is_initialized,
    // binding_count, and update_cell were removed after those methods were deleted.

    // R7RS 5.2.2: Variable definition and lookup
    #[test]
    fn test_variable_definition_and_lookup() {
        let env = Rc::new(ProcessedEnvironment::new());

        // Define a variable using extend()
        let value = ProcessedValue::Integer(42);
        let mut interner = StringInterner::<DefaultBackend>::new();
        let test_var_symbol = interner.get_or_intern("test_var");
        let env_with_var = env.extend(test_var_symbol, value);

        // Look it up
        let result = env_with_var.lookup(test_var_symbol).unwrap();
        assert!(result.is_some());

        if let Some(ProcessedValue::Integer(i)) = result {
            assert_eq!(i, 42);
        } else {
            panic!("Expected integer value");
        }
    }

    // R7RS 5.2.2: Parent-child environment and lexical scoping
    #[test]
    fn test_parent_child_environment() {
        let mut interner = StringInterner::<DefaultBackend>::new();
        let parent_env = Rc::new(ProcessedEnvironment::new());
        let parent_var = interner.get_or_intern("parent_var");
        let child_var = interner.get_or_intern("child_var");
        let parent_with_var = parent_env.extend(parent_var, ProcessedValue::Boolean(true));

        let child_env = parent_with_var.extend(child_var, ProcessedValue::Integer(10));

        // Child should have its own variables
        assert!(child_env.lookup(child_var).unwrap().is_some());

        // Child should see parent variables
        assert!(child_env.lookup(parent_var).unwrap().is_some());
    }

    // R7RS 5.2.2: Symbol-based operations and interning
    #[test]
    fn test_symbol_based_operations() {
        let env = Rc::new(ProcessedEnvironment::new());

        // Define using StringSymbol and extend()
        let symbol = StringSymbol::try_from_usize(123).unwrap();
        let value = ProcessedValue::Integer(42); // **R7RS RESTRICTED:** Only i64 integers supported
        let env_with_var = env.extend(symbol, value);

        // Look up using the same symbol
        let result = env_with_var.lookup(symbol).unwrap();
        assert!(result.is_some());

        if let Some(ProcessedValue::Integer(i)) = result {
            assert_eq!(i, 42);
        } else {
            panic!("Expected integer value");
        }
    }
    // R7RS 5.2.2: Variable shadowing and environment precedence
    #[test]
    fn test_variable_shadowing() {
        let mut interner = StringInterner::<DefaultBackend>::new();
        let parent_env = Rc::new(ProcessedEnvironment::new());
        let shared_var = interner.get_or_intern("shared_var");
        let parent_with_var = parent_env.extend(shared_var, ProcessedValue::Integer(100));

        let child_env = parent_with_var.extend(shared_var, ProcessedValue::Integer(200));

        // Child should see its own value, not parent's
        if let Some(ProcessedValue::Integer(i)) = child_env.lookup(shared_var).unwrap() {
            assert_eq!(i, 200);
        } else {
            panic!("Expected child's shadowed value");
        }
    }
}
