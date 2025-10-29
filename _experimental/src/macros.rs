// Macro expansion system - implements a subset of R7RS syntax-rules.
//
// This module serves as the integration layer for the macro system, coordinating:
// - `macro_compiler`: Definition-time compilation and validation.
// - `macro_matcher`: Runtime pattern matching with hierarchical bindings.
// - `macro_expander`: Template expansion using matched bindings.
//
// The `MacroExpander` struct drives the overall expansion process through the AST,
// handling `define-syntax` forms and iterating expansion until stable.
//
// **R7RS COMPLIANCE STATUS:**
//
// **CRITICAL GAPS:**
// 1.  **No Nested Ellipsis Support**: The system can parse and match nested ellipsis patterns
//     but fails during template expansion. This is a major functional limitation.
// 2.  **No Hygiene**: The macro system is not hygienic, meaning it is susceptible to
//     variable capture issues. This is a significant deviation from R7RS.
//
// See `MACRO_SYSTEM_RESTRICTIONS.md` for a detailed analysis.

use crate::value::{Environment, Value};
use std::collections::HashSet;
use std::rc::Rc;

pub const STANDARD_DERIVED_EXPRESSIONS: &[&str] = &[
    "and", "or", "when", "unless", "cond", "case", "let", "let*", "do",
];

// Re-export types from the new modular macro system
pub use crate::macro_compiler::{
    CompiledPattern, CompiledRule, CompiledTemplate, MacroDefinition, MacroError, MacroPattern,
    MacroTemplate, SyntaxRule,
};
pub use crate::macro_expander::expand_template;
pub use crate::macro_matcher::{match_pattern, MatchContext, MatchResult};

// Global debug flag - set via environment variable MACRO_DEBUG=1
static DEBUG: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| std::env::var("MACRO_DEBUG").is_ok());

macro_rules! debug_trace {
    ($($arg:tt)*) => {
        if *DEBUG {
            eprintln!("[MACROS DEBUG] {}", format!($($arg)*));
        }
    };
}

// Helper for compact value display in debug output
fn value_compact(v: &Value) -> String {
    match v {
        Value::Symbol(s) => s.clone(),
        Value::Integer(n) => n.to_string(),
        Value::Boolean(b) => {
            if *b {
                "#t".to_string()
            } else {
                "#f".to_string()
            }
        }
        Value::String(s) => format!("\"{}\"", s),
        Value::List(items) => {
            let item_strs: Vec<_> = items.iter().map(value_compact).collect();
            format!("({})", item_strs.join(" "))
        }
        _ => format!("{:?}", v),
    }
}

pub struct MacroExpander {
    macros: std::collections::HashMap<String, MacroDefinition>,
    known_macro_symbols: HashSet<String>,
    expansion_dirty: bool,
}

impl MacroExpander {
    pub fn new(_environment: Rc<Environment>) -> Self {
        MacroExpander {
            macros: std::collections::HashMap::new(),
            known_macro_symbols: HashSet::new(),
            expansion_dirty: false,
        }
    }

    pub fn expand(&mut self, ast: &Value) -> Result<Value, MacroError> {
        self.expand_until_stable(ast)
    }

    /// Get a macro definition by name (for debugging/introspection)
    pub fn get_macro_definition(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name)
    }

    /// Get all macro names (for debugging/introspection)
    pub fn get_macro_names(&self) -> Vec<String> {
        self.macros.keys().cloned().collect()
    }

    fn expand_until_stable(&mut self, ast: &Value) -> Result<Value, MacroError> {
        use std::borrow::Cow;
        const MAX_EXPANSIONS: usize = 100;
        let mut current = Cow::Borrowed(ast);
        let mut expansion_count = 0;

        loop {
            self.expansion_dirty = false;
            let expanded = self.expand_once(&current)?;

            if !self.expansion_dirty {
                return Ok(current.into_owned());
            }

            current = Cow::Owned(expanded);
            expansion_count += 1;

            if expansion_count > MAX_EXPANSIONS {
                return Err(MacroError(format!(
                    "Expression expansion exceeded {} iterations. This suggests infinite macro expansion.",
                    MAX_EXPANSIONS
                )));
            }
        }
    }

    fn expand_once(&mut self, ast: &Value) -> Result<Value, MacroError> {
        match ast {
            Value::List(items) if !items.is_empty() => {
                if let Value::Symbol(name) = &items[0] {
                    if name == "define-syntax" {
                        return self.handle_define_syntax(items);
                    }
                    if let Some(macro_def) = self.get_macro(name) {
                        self.expansion_dirty = true;
                        return self.expand_macro_rule(&macro_def, items);
                    }
                }

                let expanded_items: Result<Vec<_>, _> =
                    items.iter().map(|item| self.expand_once(item)).collect();

                let final_items: Vec<_> = expanded_items?
                    .into_iter()
                    .filter(|v| !matches!(v, Value::Unspecified))
                    .collect();

                if final_items != *items {
                    self.expansion_dirty = true;
                }
                Ok(Value::List(final_items))
            }
            _ => Ok(ast.clone()),
        }
    }

    fn handle_define_syntax(&mut self, items: &[Value]) -> Result<Value, MacroError> {
        if items.len() != 3 {
            return Err(MacroError(
                "define-syntax requires exactly 2 arguments".to_string(),
            ));
        }
        let name = match &items[1] {
            Value::Symbol(s) => s.clone(),
            _ => {
                return Err(MacroError(
                    "define-syntax name must be a symbol".to_string(),
                ))
            }
        };
        let macro_def = crate::macro_compiler::parse_syntax_rules(&name, &items[2])?;
        self.store_macro(macro_def)?;
        Ok(Value::Unspecified)
    }

    fn expand_macro_rule(
        &mut self,
        macro_def: &MacroDefinition,
        full_form: &[Value],
    ) -> Result<Value, MacroError> {
        debug_trace!("Expanding macro {}", macro_def.name);
        debug_trace!(
            "  Input: ({})",
            full_form
                .iter()
                .map(value_compact)
                .collect::<Vec<_>>()
                .join(" ")
        );

        for (rule_idx, compiled_rule) in macro_def.compiled_rules.iter().enumerate() {
            debug_trace!(
                "  Trying rule {}/{}",
                rule_idx + 1,
                macro_def.compiled_rules.len()
            );

            // Use the new modular matcher
            let input = Value::List(full_form.to_vec());
            match match_pattern(&compiled_rule.pattern, &input, &macro_def.literals) {
                MatchResult::Success(context) => {
                    debug_trace!("  Rule {} matched!", rule_idx + 1);

                    // Use the new modular expander
                    // Pass pattern variables (not template variables) to distinguish from literals
                    let result = expand_template(
                        &compiled_rule.template,
                        &context,
                        &compiled_rule.pattern.variables,
                    )?;
                    debug_trace!("  Expansion result: {}", value_compact(&result));
                    return Ok(result);
                }
                MatchResult::Failure(msg) => {
                    debug_trace!("  Rule {} did not match: {}", rule_idx + 1, msg);
                }
            }
        }

        Err(MacroError(format!(
            "No matching rule for macro {}",
            macro_def.name
        )))
    }

    fn store_macro(&mut self, macro_def: MacroDefinition) -> Result<(), MacroError> {
        let name = macro_def.name.clone();
        self.macros.insert(name.clone(), macro_def);
        self.known_macro_symbols.insert(name);
        Ok(())
    }

    fn get_macro(&self, name: &str) -> Option<MacroDefinition> {
        self.macros.get(name).cloned()
    }

    pub fn load_prelude(&mut self) -> Result<(), MacroError> {
        let prelude_path = "prelude/macros.scm";
        let prelude_content = std::fs::read_to_string(prelude_path)
            .map_err(|e| MacroError(format!("Failed to read prelude file: {}", e)))?;

        let expressions = crate::parser::parse_multiple(&prelude_content)
            .map_err(|e| MacroError(format!("Failed to parse prelude file: {}", e)))?;

        for expr in expressions {
            self.expand_once(&expr)?;
        }

        Ok(())
    }
}
