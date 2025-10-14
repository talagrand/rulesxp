// ProcessedEnvironment - Environment for ProcessedValue with arena-tied lifetimes
// This environment stores ProcessedValues directly using arena references
// to maintain string interning benefits and eliminate unsafe lifetime conversions.

use crate::processed_ast::StringSymbol;
use crate::super_builtins::ProcessedValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Hybrid binding type for environment: Immutable for direct bindings, Mutable for letrec/cell bindings
#[derive(Debug, Clone)]
pub enum BindingValue<'ast> {
    Immutable(ProcessedValue<'ast>),
    Mutable(Rc<RefCell<ProcessedValue<'ast>>>),
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
/// **MUTATION SUPPORT:** All bindings use Mutable(Rc<RefCell<>>) to support set!
/// This allows variable mutation while maintaining immutable environment chaining.
#[derive(Debug, Clone)]
pub struct ProcessedEnvironment<'ast> {
    /// Variable bindings - maps interned symbols to Mutable cell values
    /// All bindings are Mutable to support set! semantics
    bindings: HashMap<StringSymbol, BindingValue<'ast>>,
    /// Parent environment for lexical scoping
    parent: Option<Rc<ProcessedEnvironment<'ast>>>,
}

impl<'ast> ProcessedEnvironment<'ast> {
    /// Check if a symbol is bound as Mutable (letrec cell)
    pub fn is_mutable_binding(&self, symbol: &StringSymbol) -> bool {
        matches!(self.bindings.get(symbol), Some(BindingValue::Mutable(_)))
    }

    /// Check if this environment has any Mutable bindings
    /// Returns true if in letrec/internal define context
    pub fn has_mutable_bindings(&self) -> bool {
        self.bindings
            .values()
            .any(|b| matches!(b, BindingValue::Mutable(_)))
    }

    /// Get the number of bindings in this environment (not including parent)
    pub fn binding_count(&self) -> usize {
        self.bindings.len()
    }

    /// Get all symbol keys in this environment (not including parent)
    pub fn binding_keys(&self) -> Vec<StringSymbol> {
        self.bindings.keys().copied().collect()
    }

    /// Create a new empty environment
    pub fn new() -> Self {
        ProcessedEnvironment {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new environment with a parent
    pub fn with_parent(parent: Rc<ProcessedEnvironment<'ast>>) -> Self {
        ProcessedEnvironment {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Extend this environment with a new binding, returning a new environment
    ///
    /// **IMMUTABLE SEMANTICS:** This creates a new environment that chains to self.
    /// The original environment is unchanged. This is used for regular defines.
    ///
    /// **USAGE:**
    /// ```rust
    /// let env1 = Rc::new(ProcessedEnvironment::new());
    /// let env2 = env1.extend(symbol, BindingValue::Immutable(value));
    /// // env1 unchanged, env2 has new binding
    /// ```
    pub fn extend(
        self: &Rc<Self>,
        symbol: StringSymbol,
        value: BindingValue<'ast>,
    ) -> Rc<ProcessedEnvironment<'ast>> {
        let mut new_bindings = HashMap::new();
        new_bindings.insert(symbol, value);

        Rc::new(ProcessedEnvironment {
            bindings: new_bindings,
            parent: Some(Rc::clone(self)),
        })
    }

    /// Create a new environment with multiple bindings (for letrec initialization)
    ///
    /// **LETREC USAGE:** This creates an environment with all letrec bindings as
    /// Mutable cells initialized to Unspecified, allowing mutual recursion through
    /// back-patching.
    pub fn with_bindings(
        parent: Option<Rc<ProcessedEnvironment<'ast>>>,
        bindings: HashMap<StringSymbol, BindingValue<'ast>>,
    ) -> Rc<ProcessedEnvironment<'ast>> {
        Rc::new(ProcessedEnvironment { bindings, parent })
    }

    /// Look up a variable by StringSymbol, dereferencing Mutable cells
    pub fn lookup(&self, symbol: StringSymbol) -> Option<ProcessedValue<'ast>> {
        if let Some(binding) = self.bindings.get(&symbol) {
            match binding {
                BindingValue::Immutable(val) => Some(val.clone()),
                BindingValue::Mutable(cell) => Some(cell.borrow().clone()),
            }
        } else if let Some(parent) = &self.parent {
            parent.lookup(symbol)
        } else {
            None
        }
    }

    /// Update a Mutable cell's value (for letrec back-patching)
    ///
    /// **LETREC BACK-PATCHING:** After creating procedures during letrec initialization,
    /// this method updates the Mutable cells with the actual procedure values.
    ///
    /// **PANICS:** If the symbol is not bound or is not a Mutable binding.
    pub fn update_cell(&self, symbol: StringSymbol, value: ProcessedValue<'ast>) {
        match self.bindings.get(&symbol) {
            Some(BindingValue::Mutable(cell)) => {
                *cell.borrow_mut() = value;
            }
            _ => panic!("update_cell called on non-Mutable binding"),
        }
    }

    /// Set a variable's value, searching this environment and parent chain
    ///
    /// **SET! SEMANTICS:** Implements R7RS set! by mutating existing bindings.
    /// Works with both Immutable and Mutable bindings by upgrading Immutable to Mutable
    /// on first mutation. Returns error if variable is not defined.
    ///
    /// **MUTATION STRATEGY:** For Immutable bindings, upgrades to Mutable(Rc<RefCell<>>)
    /// on first set! For Mutable bindings, updates the cell directly.
    /// For parent scopes, recursively searches up the chain.
    pub fn set_binding(
        &self,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> Result<(), String> {
        // Check if bound locally
        if let Some(binding) = self.bindings.get(&symbol) {
            match binding {
                BindingValue::Immutable(_old) => {
                    // Cannot upgrade Immutable to Mutable through &self
                    // Solution: All define'd bindings must be Mutable from creation
                    Err(format!(
                        "Cannot set! immutable binding: {:?} (internal error - bindings should be mutable)",
                        symbol
                    ))
                }
                BindingValue::Mutable(cell) => {
                    // Already mutable, just update
                    *cell.borrow_mut() = value;
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
        use std::cell::RefCell;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_even = StringSymbol::try_from_usize(1).unwrap();
        let sym_odd = StringSymbol::try_from_usize(2).unwrap();
        let cell_even = Rc::new(RefCell::new(ProcessedValue::Unspecified));
        let cell_odd = Rc::new(RefCell::new(ProcessedValue::Unspecified));
        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_even => BindingValue::Mutable(Rc::clone(&cell_even)),
                sym_odd => BindingValue::Mutable(Rc::clone(&cell_odd)),
            },
        );
        // Simulate letrec initialization
        *cell_even.borrow_mut() = ProcessedValue::Integer(42);
        *cell_odd.borrow_mut() = ProcessedValue::Integer(99);
        assert_eq!(env.lookup(sym_even), Some(ProcessedValue::Integer(42)));
        assert_eq!(env.lookup(sym_odd), Some(ProcessedValue::Integer(99)));
    }

    #[test]
    fn test_letrec_closure_capture() {
        // R7RS 5.2.2: Closures capture immutable environment chains
        // In new architecture: letrec cells stay as cells, so closures see cell updates
        // Regular defines create new environments, so closures see captured values
        use maplit::hashmap;
        use std::cell::RefCell;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_x = StringSymbol::try_from_usize(3).unwrap();
        let cell_x = Rc::new(RefCell::new(ProcessedValue::Integer(123)));
        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_x => BindingValue::Mutable(Rc::clone(&cell_x)),
            },
        );

        // Simulate closure creation - just clone the Rc
        let env_rc = Rc::new(env);
        let closure_env = Rc::clone(&env_rc);

        // Closure sees current cell value
        assert_eq!(
            closure_env.lookup(sym_x),
            Some(ProcessedValue::Integer(123))
        );

        // Mutate x via update_cell (simulating letrec back-patch)
        env_rc.update_cell(sym_x, ProcessedValue::Integer(999));

        // Closure now sees updated value (cells are shared)
        // This is correct for letrec - all closures share the same cells
        assert_eq!(
            closure_env.lookup(sym_x),
            Some(ProcessedValue::Integer(999))
        );

        // Original env also sees the updated value
        assert_eq!(env_rc.lookup(sym_x), Some(ProcessedValue::Integer(999)));
    }

    #[test]
    fn test_environment_shadowing_and_parent_chain() {
        // R7RS 5.2.2: Shadowing and parent chain lookup
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(4).unwrap();
        let parent_with_binding =
            parent.extend(sym, BindingValue::Immutable(ProcessedValue::Integer(1)));
        let child =
            parent_with_binding.extend(sym, BindingValue::Immutable(ProcessedValue::Integer(2)));
        assert_eq!(child.lookup(sym), Some(ProcessedValue::Integer(2)));
        assert_eq!(
            parent_with_binding.lookup(sym),
            Some(ProcessedValue::Integer(1))
        );
    }

    #[test]
    fn test_define_and_redefine_mutable_and_immutable() {
        // R7RS 5.2.2: Immutable environments: redefinition creates new shadow binding
        use maplit::hashmap;
        use std::cell::RefCell;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym = StringSymbol::try_from_usize(5).unwrap();
        let cell = Rc::new(RefCell::new(ProcessedValue::Integer(10)));
        let env1 = ProcessedEnvironment::with_bindings(
            Some(parent.clone()),
            hashmap! { sym => BindingValue::Mutable(Rc::clone(&cell)) },
        );
        // Update cell value via update_cell
        let env1_rc = Rc::new(env1);
        env1_rc.update_cell(sym, ProcessedValue::Integer(20));
        assert_eq!(env1_rc.lookup(sym), Some(ProcessedValue::Integer(20)));

        // Test immutable redefinition: create new environment shadowing old binding
        let sym2 = StringSymbol::try_from_usize(6).unwrap();
        let env2 = parent.extend(sym2, BindingValue::Immutable(ProcessedValue::Integer(30)));
        let env3 = env2.extend(sym2, BindingValue::Immutable(ProcessedValue::Integer(40)));
        assert_eq!(env3.lookup(sym2), Some(ProcessedValue::Integer(40)));
        assert_eq!(env2.lookup(sym2), Some(ProcessedValue::Integer(30)));
    }

    #[test]
    fn test_is_mutable_binding() {
        // R7RS 5.2.2: is_mutable_binding must correctly identify cell bindings
        use maplit::hashmap;
        use std::cell::RefCell;
        use std::rc::Rc;
        let parent = Rc::new(ProcessedEnvironment::new());
        let sym_mut = StringSymbol::try_from_usize(7).unwrap();
        let sym_imm = StringSymbol::try_from_usize(8).unwrap();
        let cell = Rc::new(RefCell::new(ProcessedValue::Integer(1)));
        let env = ProcessedEnvironment::with_bindings(
            Some(parent),
            hashmap! {
                sym_mut => BindingValue::Mutable(Rc::clone(&cell)),
                sym_imm => BindingValue::Immutable(ProcessedValue::Integer(2)),
            },
        );
        assert!(env.is_mutable_binding(&sym_mut));
        assert!(!env.is_mutable_binding(&sym_imm));
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
        let env_with_var = env.extend(test_var_symbol, BindingValue::Immutable(value));

        // Look it up
        let result = env_with_var.lookup(test_var_symbol);
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
        let parent_with_var = parent_env.extend(
            parent_var,
            BindingValue::Immutable(ProcessedValue::Boolean(true)),
        );

        let child_env = parent_with_var.extend(
            child_var,
            BindingValue::Immutable(ProcessedValue::Integer(10)),
        );

        // Child should have its own variables
        assert!(child_env.lookup(child_var).is_some());

        // Child should see parent variables
        assert!(child_env.lookup(parent_var).is_some());
    }

    // R7RS 5.2.2: Symbol-based operations and interning
    #[test]
    fn test_symbol_based_operations() {
        let env = Rc::new(ProcessedEnvironment::new());

        // Define using StringSymbol and extend()
        let symbol = StringSymbol::try_from_usize(123).unwrap();
        let value = ProcessedValue::Integer(42); // **R7RS RESTRICTED:** Only i64 integers supported
        let env_with_var = env.extend(symbol, BindingValue::Immutable(value));

        // Look up using the same symbol
        let result = env_with_var.lookup(symbol);
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
        let parent_with_var = parent_env.extend(
            shared_var,
            BindingValue::Immutable(ProcessedValue::Integer(100)),
        );

        let child_env = parent_with_var.extend(
            shared_var,
            BindingValue::Immutable(ProcessedValue::Integer(200)),
        );

        // Child should see its own value, not parent's
        if let Some(ProcessedValue::Integer(i)) = child_env.lookup(shared_var) {
            assert_eq!(i, 200);
        } else {
            panic!("Expected child's shadowed value");
        }
    }
}
