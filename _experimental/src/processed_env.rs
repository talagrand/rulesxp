// ProcessedEnvironment - Environment for ProcessedValue with arena-tied lifetimes
// This environment stores ProcessedValues directly using arena references
// to maintain string interning benefits and eliminate unsafe lifetime conversions.

use crate::processed_ast::StringSymbol;
use crate::super_builtins::ProcessedValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(test)]
use string_interner::{DefaultBackend, StringInterner, Symbol};

/// Environment that stores ProcessedValues with arena-tied lifetimes
/// All stored values reference data in the ProcessedAST arena
/// Uses RefCell for interior mutability like the old Environment
#[derive(Debug, Clone)]
pub struct ProcessedEnvironment<'ast> {
    /// Variable bindings - maps interned symbols to arena-referenced ProcessedValues
    bindings: RefCell<HashMap<StringSymbol, ProcessedValue<'ast>>>,
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

    /// Define a variable using StringSymbol (mutates in place like old Environment)
    pub fn define(&self, symbol: StringSymbol, value: ProcessedValue<'ast>) {
        self.bindings.borrow_mut().insert(symbol, value);
    }

    /// Redefine an existing variable (for recursive function definitions)
    /// This is the same as define but semantically indicates mutation of existing binding
    pub fn redefine(&self, symbol: StringSymbol, value: ProcessedValue<'ast>) {
        self.bindings.borrow_mut().insert(symbol, value);
    }

    /// Look up a variable by StringSymbol
    pub fn lookup(&self, symbol: StringSymbol) -> Option<ProcessedValue<'ast>> {
        if let Some(value) = self.bindings.borrow().get(&symbol) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup(symbol)
        } else {
            None
        }
    }

    /// Get number of bindings in this environment (not including parent)
    pub fn binding_count(&self) -> usize {
        self.bindings.borrow().len()
    }

    /// Create a snapshot of this environment for closure capture
    ///
    /// **R7RS COMPLIANCE:** Closures must capture environment at definition time,
    /// not share mutable references that can be changed after closure creation.
    /// This method creates a flattened copy of the entire environment chain to ensure
    /// proper lexical scoping semantics.
    ///
    /// **PERFORMANCE OPTIMIZATION:** This method flattens the environment chain into
    /// a single level, which provides several benefits:
    /// - **Faster lookups**: O(1) instead of O(chain_depth)
    /// - **Memory efficiency**: Eliminates shadowed variables that would never be accessed
    /// - **Cache locality**: All bindings are in a single HashMap
    ///
    /// **FLATTENING SEMANTICS:** Variables closer to the root (more recent definitions)
    /// shadow variables with the same name in parent environments, so we traverse
    /// from leaf to root and only insert if the symbol hasn't been seen yet.
    pub fn create_snapshot(&self) -> ProcessedEnvironment<'ast> {
        // Create a new flat environment with no parent
        let snapshot = ProcessedEnvironment::new();
        let mut snapshot_bindings = snapshot.bindings.borrow_mut();

        // **FLATTENING ALGORITHM:** Traverse environment chain from leaf to root,
        // collecting all bindings but respecting shadowing (first occurrence wins)
        let mut current_env = Some(self);

        while let Some(env) = current_env {
            let current_bindings = env.bindings.borrow();

            // Insert bindings that haven't been shadowed by more recent definitions
            for (symbol, value) in current_bindings.iter() {
                // Only insert if we haven't seen this symbol yet (respects shadowing)
                snapshot_bindings
                    .entry(*symbol)
                    .or_insert_with(|| value.clone());
            }

            // Move up the chain to parent environment
            current_env = env.parent.as_ref().map(|p| p.as_ref());
        }

        drop(snapshot_bindings);

        // Return flattened environment with no parent (all bindings are at this level)
        snapshot
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

    #[test]
    fn test_environment_creation() {
        let env = ProcessedEnvironment::new();
        assert_eq!(env.binding_count(), 0);
    }

    #[test]
    fn test_variable_definition_and_lookup() {
        let mut env = ProcessedEnvironment::new();

        // Define a variable
        let value = ProcessedValue::Integer(42);
        let mut interner = StringInterner::<DefaultBackend>::new();
        let test_var_symbol = interner.get_or_intern("test_var");
        env.define(test_var_symbol, value);

        // Look it up
        let result = env.lookup(test_var_symbol);
        assert!(result.is_some());

        if let Some(ProcessedValue::Integer(i)) = result {
            assert_eq!(i, 42);
        } else {
            panic!("Expected integer value");
        }
    }

    #[test]
    fn test_parent_child_environment() {
        let mut interner = StringInterner::<DefaultBackend>::new();
        let mut parent_env = ProcessedEnvironment::new();
        let parent_var = interner.get_or_intern("parent_var");
        let child_var = interner.get_or_intern("child_var");
        parent_env.define(parent_var, ProcessedValue::Boolean(true));

        let mut child_env = ProcessedEnvironment::with_parent(Rc::new(parent_env));
        child_env.define(child_var, ProcessedValue::Integer(10));

        // Child should have its own variables
        assert!(child_env.lookup(child_var).is_some());

        // Child should see parent variables
        assert!(child_env.lookup(parent_var).is_some());
    }

    #[test]
    fn test_symbol_based_operations() {
        let mut env = ProcessedEnvironment::new();

        // Define using StringSymbol
        let symbol = StringSymbol::try_from_usize(123).unwrap();
        let value = ProcessedValue::Real(std::f64::consts::PI);
        env.define(symbol, value);

        // Look up using the same symbol
        let result = env.lookup(symbol);
        assert!(result.is_some());

        if let Some(ProcessedValue::Real(r)) = result {
            assert!((r - std::f64::consts::PI).abs() < f64::EPSILON);
        } else {
            panic!("Expected real value");
        }
    }
    #[test]
    fn test_variable_shadowing() {
        let mut interner = StringInterner::<DefaultBackend>::new();
        let mut parent_env = ProcessedEnvironment::new();
        let shared_var = interner.get_or_intern("shared_var");
        parent_env.define(shared_var, ProcessedValue::Integer(100));

        let mut child_env = ProcessedEnvironment::with_parent(Rc::new(parent_env));
        child_env.define(shared_var, ProcessedValue::Integer(200));

        // Child should see its own value, not parent's
        if let Some(ProcessedValue::Integer(i)) = child_env.lookup(shared_var) {
            assert_eq!(i, 200);
        } else {
            panic!("Expected child's shadowed value");
        }
    }
}
