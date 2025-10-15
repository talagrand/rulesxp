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
/// The BindingState tracks whether the binding can be safely read.
#[derive(Debug, Clone)]
pub struct BindingValue<'ast> {
    state: Rc<RefCell<BindingState>>,
    value: Rc<RefCell<ProcessedValue<'ast>>>,
}

impl<'ast> BindingValue<'ast> {
    /// Create a new uninitialized binding (for letrec/letrec*/internal-defines)
    pub fn new_uninitialized() -> Self {
        BindingValue {
            state: Rc::new(RefCell::new(BindingState::Uninitialized)),
            value: Rc::new(RefCell::new(ProcessedValue::Unspecified)),
        }
    }

    /// Create a new initialized binding with a value
    pub fn new_initialized(value: ProcessedValue<'ast>) -> Self {
        BindingValue {
            state: Rc::new(RefCell::new(BindingState::Initialized)),
            value: Rc::new(RefCell::new(value)),
        }
    }

    /// Get the current state
    pub fn state(&self) -> BindingState {
        *self.state.borrow()
    }

    /// Get the value (caller must check state first to enforce TDZ)
    pub fn value(&self) -> ProcessedValue<'ast> {
        self.value.borrow().clone()
    }

    /// Initialize the binding (transition from Uninitialized to Initialized)
    pub fn initialize(&self, value: ProcessedValue<'ast>) {
        *self.state.borrow_mut() = BindingState::Initialized;
        *self.value.borrow_mut() = value;
    }

    /// Update the value (for set! - requires Initialized state)
    pub fn update(&self, value: ProcessedValue<'ast>) {
        *self.value.borrow_mut() = value;
    }

    /// Get the underlying cell reference (for letrec back-patching)
    pub fn cell(&self) -> &Rc<RefCell<ProcessedValue<'ast>>> {
        &self.value
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
/// **TDZ ENFORCEMENT:** All bindings use BindingState to track initialization status.
/// Reading uninitialized bindings is an error per R7RS 5.3.2.
///
/// **REPL SEMANTICS:** The bindings HashMap is wrapped in RefCell to support top-level
/// redefinition in REPL contexts. This allows `redefine_binding` to add new bindings
/// without creating a new environment.
#[derive(Debug, Clone)]
pub struct ProcessedEnvironment<'ast> {
    /// Variable bindings - maps interned symbols to BindingValue with state tracking
    /// Wrapped in RefCell to support REPL redefinition (adding new bindings to existing env)
    bindings: RefCell<HashMap<StringSymbol, BindingValue<'ast>>>,
    /// Parent environment for lexical scoping
    parent: Option<Rc<ProcessedEnvironment<'ast>>>,
}

impl<'ast> ProcessedEnvironment<'ast> {
    /// Check if a symbol is bound (regardless of state)
    pub fn is_bound(&self, symbol: &StringSymbol) -> bool {
        self.bindings.borrow().contains_key(symbol)
    }

    /// Check if a symbol is bound and initialized
    pub fn is_initialized(&self, symbol: &StringSymbol) -> bool {
        self.bindings
            .borrow()
            .get(symbol)
            .map_or(false, |b| b.state() == BindingState::Initialized)
    }

    /// Get the number of bindings in this environment (not including parent)
    pub fn binding_count(&self) -> usize {
        self.bindings.borrow().len()
    }

    /// Get all symbol keys in this environment (not including parent)
    pub fn binding_keys(&self) -> Vec<StringSymbol> {
        self.bindings.borrow().keys().copied().collect()
    }

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
        new_bindings.insert(symbol, BindingValue::new_initialized(value));

        Rc::new(ProcessedEnvironment {
            bindings: RefCell::new(new_bindings),
            parent: Some(Rc::clone(self)),
        })
    }

    /// Create a new environment with multiple bindings (for letrec/letrec*/internal-defines)
    ///
    /// **LETREC USAGE:** This creates an environment with all bindings in the specified state.
    /// For letrec: all bindings start Uninitialized, get initialized sequentially.
    /// For internal defines: same as letrec*.
    pub fn with_bindings(
        parent: Option<Rc<ProcessedEnvironment<'ast>>>,
        bindings: HashMap<StringSymbol, BindingValue<'ast>>,
    ) -> Rc<ProcessedEnvironment<'ast>> {
        Rc::new(ProcessedEnvironment {
            bindings: RefCell::new(bindings),
            parent,
        })
    }

    /// Create a new environment with multiple uninitialized bindings from symbol list
    ///
    /// **CONVENIENCE METHOD:** Creates an environment with all specified symbols as
    /// Uninitialized bindings. This is the preferred method for letrec/letrec*/internal-defines
    /// setup phases.
    ///
    /// **USAGE:**
    /// ```rust
    /// let symbols = vec![sym_x, sym_y, sym_z];
    /// let env = ProcessedEnvironment::create_with_bindings(Some(parent_env), &symbols);
    /// // All symbols are now bound but Uninitialized
    /// env.initialize_binding(sym_x, value_x)?;
    /// env.initialize_binding(sym_y, value_y)?;
    /// // etc.
    /// ```
    pub fn create_with_bindings(
        parent: Option<Rc<ProcessedEnvironment<'ast>>>,
        symbols: &[StringSymbol],
    ) -> Rc<ProcessedEnvironment<'ast>> {
        let mut bindings = HashMap::new();
        for &symbol in symbols {
            bindings.insert(symbol, BindingValue::new_uninitialized());
        }
        Rc::new(ProcessedEnvironment {
            bindings: RefCell::new(bindings),
            parent,
        })
    }

    /// Define a new uninitialized binding in a new environment (for letrec/letrec*/internal-defines setup)
    ///
    /// **TDZ SEMANTICS:** Creates binding in Uninitialized state. Must call initialize_binding
    /// before the binding can be read.
    ///
    /// **USAGE:**
    /// ```rust
    /// let env1 = Rc::new(ProcessedEnvironment::new());
    /// let env2 = env1.define_binding(symbol); // Uninitialized
    /// env2.initialize_binding(symbol, value)?; // Now initialized
    /// ```
    pub fn define_binding(self: &Rc<Self>, symbol: StringSymbol) -> Rc<ProcessedEnvironment<'ast>> {
        let mut new_bindings = HashMap::new();
        new_bindings.insert(symbol, BindingValue::new_uninitialized());

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
            binding.initialize(value);
            Ok(())
        } else {
            Err(format!(
                "Cannot initialize non-existent binding: {:?}",
                symbol
            ))
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
            *binding.value.borrow_mut() = value;
            *binding.state.borrow_mut() = BindingState::Initialized;
            Ok(())
        } else {
            // Binding doesn't exist - create it (first-time define at top-level)
            let binding_value = BindingValue::new_initialized(value);
            self.bindings.borrow_mut().insert(symbol, binding_value);
            Ok(())
        }
    }

    /// Look up a variable by StringSymbol with TDZ enforcement
    ///
    /// **TDZ ENFORCEMENT:** Returns error if binding exists but is Uninitialized.
    /// Returns None if binding doesn't exist in this environment or parent chain.
    pub fn lookup(&self, symbol: StringSymbol) -> Result<Option<ProcessedValue<'ast>>, String> {
        if let Some(binding) = self.bindings.borrow().get(&symbol).cloned() {
            match binding.state() {
                BindingState::Uninitialized => Err(format!(
                    "Variable used before initialization (TDZ violation): {:?}",
                    symbol
                )),
                BindingState::Initialized => Ok(Some(binding.value())),
            }
        } else if let Some(parent) = &self.parent {
            parent.lookup(symbol)
        } else {
            Ok(None)
        }
    }

    /// Look up a variable without TDZ enforcement (for internal VM use)
    ///
    /// **INTERNAL USE ONLY:** Used during letrec back-patching where we need to access
    /// the cell even if uninitialized. Normal code should use lookup().
    pub fn lookup_unchecked(&self, symbol: StringSymbol) -> Option<ProcessedValue<'ast>> {
        if let Some(binding) = self.bindings.borrow().get(&symbol).cloned() {
            Some(binding.value())
        } else if let Some(parent) = &self.parent {
            parent.lookup_unchecked(symbol)
        } else {
            None
        }
    }

    /// Update a binding's value directly (for letrec back-patching)
    ///
    /// **LETREC BACK-PATCHING:** Used during letrec evaluation to update closures
    /// with the correct environment after all bindings are created.
    ///
    /// **PANICS:** If the symbol is not bound in this environment.
    pub fn update_cell(&self, symbol: StringSymbol, value: ProcessedValue<'ast>) {
        match self.bindings.borrow().get(&symbol).cloned() {
            Some(binding) => {
                binding.update(value);
            }
            None => panic!("update_cell called on non-existent binding"),
        }
    }

    /// Get the cell reference for a binding (for letrec closure capture)
    ///
    /// **LETREC USAGE:** Returns the underlying Rc<RefCell<>> for a binding,
    /// allowing closures to capture the cell for mutual recursion.
    pub fn get_cell(&self, symbol: StringSymbol) -> Option<Rc<RefCell<ProcessedValue<'ast>>>> {
        self.bindings
            .borrow()
            .get(&symbol)
            .map(|b| Rc::clone(b.cell()))
    }

    /// Set a variable's value, searching this environment and parent chain
    ///
    /// **SET! SEMANTICS:** Implements R7RS set! by mutating existing Initialized bindings.
    /// Returns error if variable is not defined or is Uninitialized (TDZ violation).
    ///
    /// **TDZ ENFORCEMENT:** Cannot set! an uninitialized variable.
    pub fn set_binding(
        &self,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Result<(), String> {
        // Check if bound locally
        if let Some(binding) = self.bindings.borrow().get(&symbol).cloned() {
            match binding.state() {
                BindingState::Uninitialized => Err(format!(
                    "Cannot set! uninitialized variable (TDZ violation): {:?}",
                    symbol
                )),
                BindingState::Initialized => {
                    binding.update(value);
                    Ok(())
                }
            }
        } else if let Some(parent) = &self.parent {
            // Recursively try parent
            parent.set_binding(symbol, value)
        } else {
            Err(format!("Undefined variable for set!: {:?}", symbol))
        }
    }
}

impl<'ast> Default for ProcessedEnvironment<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    // === R7RS 5.2.2: Environment and Variable Binding Edge Cases ===

    #[test]
    fn test_letrec_mutual_recursion_cells() {
        // R7RS 5.2.2: letrec mutual recursion must work via cells
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_even = StringSymbol::try_from_usize(1).unwrap();
        let sym_odd = StringSymbol::try_from_usize(2).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_even => BindingValue::new_uninitialized(),
                sym_odd => BindingValue::new_uninitialized(),
            },
        );

        // Simulate letrec initialization
        env.initialize_binding(sym_even, ProcessedValue::Integer(42))
            .unwrap();
        env.initialize_binding(sym_odd, ProcessedValue::Integer(99))
            .unwrap();

        assert_eq!(
            env.lookup(sym_even).unwrap(),
            Some(ProcessedValue::Integer(42))
        );
        assert_eq!(
            env.lookup(sym_odd).unwrap(),
            Some(ProcessedValue::Integer(99))
        );
    }

    #[test]
    fn test_letrec_closure_capture() {
        // R7RS 5.2.2: Closures capture immutable environment chains
        // In new architecture: letrec cells stay as cells, so closures see cell updates
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_x = StringSymbol::try_from_usize(3).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_x => BindingValue::new_uninitialized(),
            },
        );

        // Initialize binding
        env.initialize_binding(sym_x, ProcessedValue::Integer(123))
            .unwrap();

        // Simulate closure creation - just clone the Rc
        let env_rc = Rc::new(env);
        let closure_env = Rc::clone(&env_rc);

        // Closure sees current cell value
        assert_eq!(
            closure_env.lookup(sym_x).unwrap(),
            Some(ProcessedValue::Integer(123))
        );

        // Mutate x via update_cell (simulating letrec back-patch)
        env_rc.update_cell(sym_x, ProcessedValue::Integer(999));

        // Closure now sees updated value (cells are shared)
        // This is correct for letrec - all closures share the same cells
        assert_eq!(
            closure_env.lookup(sym_x).unwrap(),
            Some(ProcessedValue::Integer(999))
        );

        // Original env also sees the updated value
        assert_eq!(
            env_rc.lookup(sym_x).unwrap(),
            Some(ProcessedValue::Integer(999))
        );
    }

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

    #[test]
    fn test_define_and_redefine_mutable_and_immutable() {
        // R7RS 5.2.2: Immutable environments: redefinition creates new shadow binding
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(5).unwrap();

        let env1 = ProcessedEnvironment::with_bindings(
            Some(parent.clone()),
            hashmap! { sym => BindingValue::new_uninitialized() },
        );

        // Initialize binding
        env1.initialize_binding(sym, ProcessedValue::Integer(10))
            .unwrap();
        assert_eq!(env1.lookup(sym).unwrap(), Some(ProcessedValue::Integer(10)));

        // Update via update_cell
        env1.update_cell(sym, ProcessedValue::Integer(20));
        assert_eq!(env1.lookup(sym).unwrap(), Some(ProcessedValue::Integer(20)));

        // Test immutable redefinition: create new environment shadowing old binding
        let sym2 = StringSymbol::try_from_usize(6).unwrap();
        let env2 = parent.extend(sym2, ProcessedValue::Integer(30));
        let env3 = env2.extend(sym2, ProcessedValue::Integer(40));
        assert_eq!(
            env3.lookup(sym2).unwrap(),
            Some(ProcessedValue::Integer(40))
        );
        assert_eq!(
            env2.lookup(sym2).unwrap(),
            Some(ProcessedValue::Integer(30))
        );
    }

    #[test]
    fn test_is_mutable_binding() {
        // R7RS 5.2.2: Check binding state
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_uninit = StringSymbol::try_from_usize(7).unwrap();
        let sym_init = StringSymbol::try_from_usize(8).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_uninit => BindingValue::new_uninitialized(),
                sym_init => BindingValue::new_initialized(ProcessedValue::Integer(2)),
            },
        );

        assert!(env.is_bound(&sym_uninit));
        assert!(!env.is_initialized(&sym_uninit));

        assert!(env.is_bound(&sym_init));
        assert!(env.is_initialized(&sym_init));
    }
    use super::*;

    // R7RS 5.2.2: Environment creation and empty bindings
    #[test]
    fn test_environment_creation() {
        let env = ProcessedEnvironment::new();
        assert_eq!(env.binding_count(), 0);
    }

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

    // R7RS 5.3.2: TDZ enforcement - uninitialized variable read
    #[test]
    fn test_tdz_uninitialized_read() {
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(100).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym => BindingValue::new_uninitialized(),
            },
        );

        // Reading uninitialized binding should error
        let result = env.lookup(sym);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("TDZ violation"));
    }

    // R7RS 5.3.2: TDZ enforcement - cannot set! uninitialized variable
    #[test]
    fn test_tdz_uninitialized_set() {
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(101).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym => BindingValue::new_uninitialized(),
            },
        );

        // Setting uninitialized binding should error
        let result = env.set_binding(sym, ProcessedValue::Integer(42));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("TDZ violation"));
    }

    // R7RS 5.3.2: TDZ lifecycle - uninitialized -> initialized -> mutated
    #[test]
    fn test_tdz_lifecycle() {
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(102).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym => BindingValue::new_uninitialized(),
            },
        );

        // State 1: Uninitialized - cannot read
        assert!(env.lookup(sym).is_err());

        // State 2: Initialize
        env.initialize_binding(sym, ProcessedValue::Integer(10))
            .unwrap();
        assert_eq!(env.lookup(sym).unwrap(), Some(ProcessedValue::Integer(10)));

        // State 3: Mutate with set!
        env.set_binding(sym, ProcessedValue::Integer(20)).unwrap();
        assert_eq!(env.lookup(sym).unwrap(), Some(ProcessedValue::Integer(20)));
    }

    // R7RS 5.3.2: REPL redefinition
    #[test]
    fn test_repl_redefinition() {
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(103).unwrap();

        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym => BindingValue::new_initialized(ProcessedValue::Integer(10)),
            },
        );

        assert_eq!(env.lookup(sym).unwrap(), Some(ProcessedValue::Integer(10)));

        // Redefine binding
        env.redefine_binding(sym, ProcessedValue::Integer(20))
            .unwrap();
        assert_eq!(env.lookup(sym).unwrap(), Some(ProcessedValue::Integer(20)));
    }

    // R7RS 5.3.2: create_with_bindings convenience method
    #[test]
    fn test_create_with_bindings() {
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_x = StringSymbol::try_from_usize(104).unwrap();
        let sym_y = StringSymbol::try_from_usize(105).unwrap();
        let sym_z = StringSymbol::try_from_usize(106).unwrap();

        // Create environment with 3 uninitialized bindings
        let symbols = vec![sym_x, sym_y, sym_z];
        let env = ProcessedEnvironment::create_with_bindings(Some(parent), &symbols);

        // All should be bound but uninitialized
        assert!(env.is_bound(&sym_x));
        assert!(env.is_bound(&sym_y));
        assert!(env.is_bound(&sym_z));
        assert!(!env.is_initialized(&sym_x));
        assert!(!env.is_initialized(&sym_y));
        assert!(!env.is_initialized(&sym_z));

        // Reading should error (TDZ)
        assert!(env.lookup(sym_x).is_err());

        // Initialize left-to-right (letrec* style)
        env.initialize_binding(sym_x, ProcessedValue::Integer(1))
            .unwrap();
        env.initialize_binding(sym_y, ProcessedValue::Integer(2))
            .unwrap();
        env.initialize_binding(sym_z, ProcessedValue::Integer(3))
            .unwrap();

        // Now all should be readable
        assert_eq!(env.lookup(sym_x).unwrap(), Some(ProcessedValue::Integer(1)));
        assert_eq!(env.lookup(sym_y).unwrap(), Some(ProcessedValue::Integer(2)));
        assert_eq!(env.lookup(sym_z).unwrap(), Some(ProcessedValue::Integer(3)));
    }

    // R7RS 5.3.2: REPL first-time define (RefCell borrow conflict regression test)
    // This test exercises the borrow conflict that occurred before adding explicit scope blocks.
    // Without the fix, redefine_binding would panic with "already borrowed" when the symbol
    // doesn't exist and needs to be created (first-time define at top-level).
    #[test]
    fn test_repl_first_time_define() {
        use maplit::hashmap;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_new = StringSymbol::try_from_usize(107).unwrap();

        // Start with empty environment
        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {}, // No existing bindings
        );

        // First-time define - this used to cause RefCell "already borrowed" panic
        // because redefine_binding would:
        // 1. Call self.bindings.borrow().get(&symbol) in if-let
        // 2. Borrow not dropped before else branch
        // 3. else branch calls self.bindings.borrow_mut().insert() - PANIC!
        env.redefine_binding(sym_new, ProcessedValue::Integer(42))
            .unwrap();

        // Should be readable now
        assert_eq!(
            env.lookup(sym_new).unwrap(),
            Some(ProcessedValue::Integer(42))
        );

        // Second define should update, not create
        env.redefine_binding(sym_new, ProcessedValue::Integer(99))
            .unwrap();
        assert_eq!(
            env.lookup(sym_new).unwrap(),
            Some(ProcessedValue::Integer(99))
        );
    }
}
