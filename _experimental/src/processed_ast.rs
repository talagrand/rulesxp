//! # ProcessedAST Compiler
//!
//! This module provides a compile-time transformation from the standard AST (`Value`)
//! to an optimized representation (`ProcessedValue`) that enables faster evaluation
//! in the SuperVM.
//!
//! ## Key Features
//!
//! - **Arena allocation**: All values allocated in a single arena for memory efficiency
//! - **String interning**: Symbols are interned to reduce memory usage and enable fast comparisons
//! - **Builtin resolution**: Function symbols resolved to direct builtin references at compile time
//! - **Type safety**: ProcessedValue provides stronger type guarantees than the generic Value enum
//!
//! ## Architecture
//!
//! The compiler transforms `Value` trees into `ProcessedValue` trees with the following mappings:
//! - Integer/Boolean/String literals → Direct ProcessedValue equivalents
//! - Symbols → Interned symbols or resolved builtin function references
//! - Lists → Compiled special forms or function applications
//! - Special forms (if, define, lambda, quote, begin) → Specialized ProcessedValue variants
//!
//! ## R7RS Special Form Support
//!
//! **Implemented Special Forms** (compiled directly by ProcessedAST):
//! - `if` - Conditional expressions with optional else branch
//! - `define` - Variable and function definitions with R7RS syntax support
//! - `lambda` - Function creation with lexical scoping
//! - `quote` - Literal data without evaluation
//! - `begin` - Sequential expression evaluation
//!
//! **R7RS DEVIATION:** Derived expressions handled by macro system (NOT ProcessedAST):
//! - `and`, `or` - Logical operators (must be macro-expanded first)
//! - `when`, `unless` - Simple conditionals (must be macro-expanded first)
//! - `cond`, `case` - Multi-way conditionals (must be macro-expanded first)
//! - `let`, `let*`, `do` - Local binding and iteration (must be macro-expanded first)
//!
//! **R7RS RESTRICTED:** Unsupported special forms:
//! - `letrec` - Mutual recursion not implemented
//! - `set!` - Variable mutation not supported (immutable bindings only)
//! - `call/cc`, `call-with-current-continuation` - Continuations not implemented
//! - `dynamic-wind` - Dynamic extent control not implemented
//! - `let-syntax`, `letrec-syntax` - Local macro definitions not supported
//! - Variadic lambda forms: `(lambda args body)` and `(lambda (a b . rest) body)`
//!
//! **CRITICAL:** Input to ProcessedAST must be fully macro-expanded. The macro system
//! (see `macros.rs`) handles derived expressions before ProcessedAST compilation.
//!
//! ## Usage
//!
//! ```rust
//! let ast = ProcessedAST::compile(&value)?;
//! let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
//! let result = vm.evaluate(env)?;
//! ```

use crate::super_builtins::{builtin_functions, ProcessedArity, ProcessedValue};
use crate::value::Value;
use bumpalo::Bump;
use std::borrow::Cow;
use std::collections::HashMap;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner, Symbol};

/// Type alias for string symbols
pub type StringSymbol = DefaultSymbol;

/// ProcessedAST with arena allocation and string interning
/// **NEEDS-FIX:** Using unsafe transmute for lifetime management - requires proper solution
pub struct ProcessedAST {
    /// String interner for symbols and strings
    pub interner: StringInterner<DefaultBackend>,
    /// Bump arena for all allocations
    pub arena: Bump,
    /// Root expression of the AST
    /// **UNSAFE:** Lifetime transmuted to 'static - arena must outlive all references
    pub root: ProcessedValue<'static>,
}

/// Compilation error for ProcessedAST creation
#[derive(Debug, Clone)]
pub struct ProcessedCompileError {
    pub message: String,
    pub source_expression: Option<String>,
}

impl ProcessedCompileError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            source_expression: None,
        }
    }

    pub fn with_source(mut self, expr: &Value) -> Self {
        self.source_expression = Some(format!("{}", expr));
        self
    }
}

impl std::fmt::Display for ProcessedCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ProcessedAST compilation error: {}", self.message)?;
        if let Some(expr) = &self.source_expression {
            write!(f, "\n  Source: {}", expr)?;
        }
        Ok(())
    }
}

impl std::error::Error for ProcessedCompileError {}

/// Compiler context for ProcessedAST creation
struct ProcessedCompiler<'arena> {
    interner: &'arena mut StringInterner<DefaultBackend>,
    arena: &'arena Bump,
    builtins: HashMap<StringSymbol, ProcessedValue<'arena>>,
}

impl ProcessedAST {
    /// Create a new ProcessedAST from a Value expression
    pub fn compile(value: &Value) -> Result<Self, ProcessedCompileError> {
        let mut interner = StringInterner::new();
        let arena = Bump::new();

        // Create builtin function mappings
        let mut builtins = HashMap::new();

        // Add arithmetic builtins
        let add_sym = interner.get_or_intern("+");
        let sub_sym = interner.get_or_intern("-");
        let mul_sym = interner.get_or_intern("*");
        let div_sym = interner.get_or_intern("/");
        let eq_sym = interner.get_or_intern("=");
        let lt_sym = interner.get_or_intern("<");
        let gt_sym = interner.get_or_intern(">");
        let le_sym = interner.get_or_intern("<=");
        let ge_sym = interner.get_or_intern(">=");
        let not_sym = interner.get_or_intern("not");
        let car_sym = interner.get_or_intern("car");
        let cdr_sym = interner.get_or_intern("cdr");
        let cons_sym = interner.get_or_intern("cons");
        let list_sym = interner.get_or_intern("list");

        builtins.insert(
            add_sym,
            ProcessedValue::ResolvedBuiltin {
                name: add_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::add_super,
            },
        );

        builtins.insert(
            sub_sym,
            ProcessedValue::ResolvedBuiltin {
                name: sub_sym,
                arity: ProcessedArity::AtLeast(1),
                func: builtin_functions::sub_super,
            },
        );

        builtins.insert(
            mul_sym,
            ProcessedValue::ResolvedBuiltin {
                name: mul_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::mul_super,
            },
        );

        builtins.insert(
            eq_sym,
            ProcessedValue::ResolvedBuiltin {
                name: eq_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::eq_super,
            },
        );

        builtins.insert(
            div_sym,
            ProcessedValue::ResolvedBuiltin {
                name: div_sym,
                arity: ProcessedArity::AtLeast(1),
                func: builtin_functions::div_super,
            },
        );

        builtins.insert(
            lt_sym,
            ProcessedValue::ResolvedBuiltin {
                name: lt_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::lt_super,
            },
        );

        builtins.insert(
            gt_sym,
            ProcessedValue::ResolvedBuiltin {
                name: gt_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::gt_super,
            },
        );

        builtins.insert(
            le_sym,
            ProcessedValue::ResolvedBuiltin {
                name: le_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::le_super,
            },
        );

        builtins.insert(
            ge_sym,
            ProcessedValue::ResolvedBuiltin {
                name: ge_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::ge_super,
            },
        );

        builtins.insert(
            not_sym,
            ProcessedValue::ResolvedBuiltin {
                name: not_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::not_super,
            },
        );

        builtins.insert(
            car_sym,
            ProcessedValue::ResolvedBuiltin {
                name: car_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::car_super,
            },
        );

        builtins.insert(
            cdr_sym,
            ProcessedValue::ResolvedBuiltin {
                name: cdr_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::cdr_super,
            },
        );

        builtins.insert(
            cons_sym,
            ProcessedValue::ResolvedBuiltin {
                name: cons_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::cons_super,
            },
        );

        builtins.insert(
            list_sym,
            ProcessedValue::ResolvedBuiltin {
                name: list_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::list_super,
            },
        );

        // Create compiler context
        let mut compiler = ProcessedCompiler {
            interner: &mut interner,
            arena: &arena,
            builtins,
        };

        // Compile the root expression
        let root = compiler.compile_value(value)?;

        // Due to lifetime constraints, we need to use unsafe here
        // This is safe because the arena owns all the data and the ProcessedAST
        // owns the arena, so the lifetimes are actually correct
        let root_static =
            unsafe { std::mem::transmute::<ProcessedValue<'_>, ProcessedValue<'static>>(root) };

        Ok(ProcessedAST {
            interner,
            arena,
            root: root_static,
        })
    }

    /// Get the root expression with proper lifetime
    pub fn root(&self) -> &ProcessedValue<'static> {
        &self.root
    }

    /// Resolve a string symbol to its string value
    pub fn resolve_symbol(&self, symbol: StringSymbol) -> Option<&str> {
        self.interner.resolve(symbol)
    }

    /// Debug dump the ProcessedAST tree structure
    pub fn debug_dump(&self) -> String {
        self.dump_value(&self.root, 0)
    }

    /// Recursively dump a ProcessedValue with indentation
    fn dump_value(&self, value: &ProcessedValue, indent: usize) -> String {
        let prefix = "  ".repeat(indent);
        match value {
            ProcessedValue::Boolean(b) => format!("{}Boolean({})", prefix, b),
            ProcessedValue::Integer(n) => format!("{}Integer({})", prefix, n),
            ProcessedValue::UInteger(n) => format!("{}UInteger({})", prefix, n),
            ProcessedValue::Real(r) => format!("{}Real({})", prefix, r),
            ProcessedValue::String(sym) => {
                let s = self.interner.resolve(*sym).unwrap_or("<unresolved>");
                format!("{}String(\"{}\")", prefix, s)
            }
            ProcessedValue::OwnedString(s) => format!("{}OwnedString(\"{}\")", prefix, s),
            ProcessedValue::Symbol(sym) => {
                let s = self.interner.resolve(*sym).unwrap_or("<unresolved>");
                format!("{}Symbol({})", prefix, s)
            }
            ProcessedValue::OwnedSymbol(s) => format!("{}OwnedSymbol({})", prefix, s),
            ProcessedValue::List(items) => {
                let mut result = format!("{}List[\n", prefix);
                for item in items.iter() {
                    result.push_str(&self.dump_value(item, indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::ResolvedBuiltin { name, arity, .. } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                format!(
                    "{}ResolvedBuiltin({}, arity: {:?})",
                    prefix, name_str, arity
                )
            }
            ProcessedValue::Procedure {
                params,
                body,
                variadic,
                ..
            } => {
                let mut result = format!("{}Procedure[\n", prefix);
                result.push_str(&format!("{}  params: [", prefix));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    let param_str = self.interner.resolve(*param).unwrap_or("<unresolved>");
                    result.push_str(param_str);
                }
                result.push_str(&format!("], variadic: {}\n", variadic));
                result.push_str(&format!("{}  body:\n", prefix));
                result.push_str(&self.dump_value(body, indent + 2));
                result.push('\n');
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::If {
                test,
                then_branch,
                else_branch,
            } => {
                let mut result = format!("{}If[\n", prefix);
                result.push_str(&format!("{}  test:\n", prefix));
                result.push_str(&self.dump_value(test, indent + 2));
                result.push('\n');
                result.push_str(&format!("{}  then:\n", prefix));
                result.push_str(&self.dump_value(then_branch, indent + 2));
                result.push('\n');
                if let Some(else_val) = else_branch {
                    result.push_str(&format!("{}  else:\n", prefix));
                    result.push_str(&self.dump_value(else_val, indent + 2));
                    result.push('\n');
                }
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::Define { name, value } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                let mut result = format!("{}Define[{}]\n", prefix, name_str);
                result.push_str(&format!("{}  value:\n", prefix));
                result.push_str(&self.dump_value(value, indent + 2));
                result
            }
            ProcessedValue::Lambda {
                params,
                body,
                variadic,
            } => {
                let mut result = format!("{}Lambda[\n", prefix);
                result.push_str(&format!("{}  params: [", prefix));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    let param_str = self.interner.resolve(*param).unwrap_or("<unresolved>");
                    result.push_str(param_str);
                }
                result.push_str(&format!("], variadic: {}\n", variadic));
                result.push_str(&format!("{}  body:\n", prefix));
                result.push_str(&self.dump_value(body, indent + 2));
                result.push('\n');
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::Quote { value } => {
                let mut result = format!("{}Quote[\n", prefix);
                result.push_str(&self.dump_value(value, indent + 1));
                result.push('\n');
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::Begin { expressions } => {
                let mut result = format!("{}Begin[\n", prefix);
                for expr in expressions.iter() {
                    result.push_str(&self.dump_value(expr, indent + 1));
                    result.push('\n');
                }
                result.push_str(&format!("{}]", prefix));
                result
            }
            ProcessedValue::Unspecified => format!("{}Unspecified", prefix),
        }
    }
}

impl<'arena> ProcessedCompiler<'arena> {
    /// Compile a Value to ProcessedValue
    fn compile_value(
        &mut self,
        value: &Value,
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        match value {
            // Literals compile directly
            Value::Boolean(b) => Ok(ProcessedValue::Boolean(*b)),
            Value::Integer(n) => Ok(ProcessedValue::Integer(*n)),
            Value::UInteger(n) => Ok(ProcessedValue::UInteger(*n)),
            Value::Real(r) => Ok(ProcessedValue::Real(*r)),

            // Strings get interned
            Value::String(s) => {
                let symbol = self.interner.get_or_intern(s);
                Ok(ProcessedValue::String(symbol))
            }

            // Symbols get interned and checked for builtins
            Value::Symbol(s) => {
                let symbol = self.interner.get_or_intern(s);

                // Check if this is a builtin function
                if let Some(builtin) = self.builtins.get(&symbol) {
                    Ok(builtin.clone())
                } else {
                    Ok(ProcessedValue::Symbol(symbol))
                }
            }

            // Lists are either special forms or function applications
            Value::List(elements) if !elements.is_empty() => {
                // Check for special forms first
                if let Value::Symbol(first) = &elements[0] {
                    match first.as_str() {
                        "if" => self.compile_if(elements),
                        "define" => self.compile_define(elements),
                        "lambda" => self.compile_lambda(elements),
                        "quote" => self.compile_quote(elements),
                        "begin" => self.compile_begin(elements),
                        // **R7RS DEVIATION:** The following standard R7RS special forms are NOT implemented in ProcessedAST
                        // These forms are handled by the macro system instead (see prelude/macros.scm):
                        "and" | "or" | "when" | "unless" | "cond" | "case" | "let" | "let*" | "do" => {
                            return Err(ProcessedCompileError::new(format!(
                                "R7RS DEVIATION: {} is a derived expression handled by macro system, not ProcessedAST compiler. \
                                 Must be macro-expanded before ProcessedAST compilation.", first
                            )))
                        }
                        // **R7RS RESTRICTED:** The following forms are not supported at all:
                        "letrec" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: letrec requires mutual recursion support not implemented in ProcessedAST".to_string()
                            ))
                        }
                        "set!" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: Variable mutation (set!) not supported - all bindings are immutable".to_string()
                            ))
                        }
                        "call/cc" | "call-with-current-continuation" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: Continuations (call/cc) not implemented in ProcessedAST".to_string()
                            ))
                        }
                        "dynamic-wind" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: dynamic-wind not implemented in ProcessedAST".to_string()
                            ))
                        }
                        "let-syntax" | "letrec-syntax" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: Local macro definitions not supported in ProcessedAST".to_string()
                            ))
                        }
                        "define-syntax" => {
                            return Err(ProcessedCompileError::new(
                                "R7RS DEVIATION: Macro definitions should be handled by macro system before ProcessedAST compilation".to_string()
                            ))
                        }
                        _ => self.compile_list(elements), // Regular list/application
                    }
                } else {
                    self.compile_list(elements)
                }
            }

            // Empty list
            Value::List(_) => {
                // Create empty slice using alloc method instead of alloc_slice_copy
                let empty_slice: &[ProcessedValue] = &[];
                Ok(ProcessedValue::List(Cow::Borrowed(empty_slice)))
            }

            // Other values (Builtin, Procedure, Unspecified)
            Value::Builtin { name, .. } => {
                // This should have been resolved during environment setup
                Err(
                    ProcessedCompileError::new(format!("Unresolved builtin function: {}", name))
                        .with_source(value),
                )
            }

            Value::Procedure { .. } => {
                // User procedures need special handling - for now, error
                Err(ProcessedCompileError::new(
                    "User procedures not yet supported in ProcessedAST".to_string(),
                )
                .with_source(value))
            }

            Value::Unspecified => Ok(ProcessedValue::Unspecified),
        }
    }

    /// Compile if special form
    fn compile_if(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() < 3 || elements.len() > 4 {
            return Err(ProcessedCompileError::new(
                "if requires 2 or 3 arguments".to_string(),
            ));
        }

        let test = self.arena.alloc(self.compile_value(&elements[1])?);
        let then_branch = self.arena.alloc(self.compile_value(&elements[2])?);
        let else_branch = if elements.len() == 4 {
            Some(self.arena.alloc(self.compile_value(&elements[3])?) as &ProcessedValue<'arena>)
        } else {
            None
        };

        Ok(ProcessedValue::If {
            test: test as &ProcessedValue<'arena>,
            then_branch: then_branch as &ProcessedValue<'arena>,
            else_branch,
        })
    }

    /// Compile define special form
    fn compile_define(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() != 3 {
            return Err(ProcessedCompileError::new(
                "define requires exactly 2 arguments".to_string(),
            ));
        }

        let (name, value_expr) = match &elements[1] {
            // Variable definition: (define var expr)
            Value::Symbol(s) => {
                let name = self.interner.get_or_intern(s);
                (name, &elements[2])
            }
            // Function definition: (define (func args...) body...)
            Value::List(func_def) if !func_def.is_empty() => {
                match &func_def[0] {
                    Value::Symbol(func_name) => {
                        let name = self.interner.get_or_intern(func_name);

                        // Extract parameters: (func arg1 arg2 ...) -> (arg1 arg2 ...)
                        let params = func_def[1..].to_vec();

                        // Compile lambda directly without creating temporary Value
                        // This is equivalent to: (lambda (args...) body...)
                        let lambda_elements = vec![
                            Value::Symbol("lambda".to_string()),
                            Value::List(params),
                            elements[2].clone(), // body
                        ];

                        let lambda_value = self.compile_lambda(&lambda_elements)?;
                        let value = self.arena.alloc(lambda_value);

                        return Ok(ProcessedValue::Define { name, value });
                    }
                    _ => {
                        return Err(ProcessedCompileError::new(
                            "Function name in define must be a symbol".to_string(),
                        ))
                    }
                }
            }
            _ => {
                return Err(ProcessedCompileError::new(
                    "define first argument must be a symbol or function definition list"
                        .to_string(),
                ))
            }
        };

        let value = self.arena.alloc(self.compile_value(value_expr)?);

        Ok(ProcessedValue::Define { name, value })
    }

    /// Compile lambda special form
    fn compile_lambda(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() < 3 {
            return Err(ProcessedCompileError::new(
                "lambda requires at least 2 arguments".to_string(),
            ));
        }

        let params = match &elements[1] {
            Value::List(param_list) => {
                let mut param_symbols = Vec::new();
                for param in param_list {
                    if let Value::Symbol(name) = param {
                        param_symbols.push(self.interner.get_or_intern(name));
                    } else {
                        return Err(ProcessedCompileError::new(
                            "lambda parameters must be symbols".to_string(),
                        ));
                    }
                }
                param_symbols
            }
            Value::Symbol(single_param) => {
                vec![self.interner.get_or_intern(single_param)]
            }
            _ => {
                return Err(ProcessedCompileError::new(
                    "lambda parameters must be symbols or list of symbols".to_string(),
                ))
            }
        };

        let body = if elements.len() == 3 {
            self.arena.alloc(self.compile_value(&elements[2])?)
        } else {
            // Multiple body expressions - compile as begin
            let _begin_symbol = self.interner.get_or_intern("begin");
            let mut begin_exprs = Vec::new();
            for expr in &elements[2..] {
                begin_exprs.push(self.compile_value(expr)?);
            }
            // Move data into arena-allocated slice
            let begin_slice = self.arena.alloc_slice_fill_iter(begin_exprs.into_iter());
            self.arena.alloc(ProcessedValue::Begin {
                expressions: Cow::Borrowed(begin_slice),
            })
        };

        let params_slice = self.arena.alloc_slice_fill_iter(params.into_iter());

        Ok(ProcessedValue::Lambda {
            params: Cow::Borrowed(params_slice),
            body,
            variadic: false, // **R7RS RESTRICTED:** Variadic functions (lambda args body) and (lambda (a b . rest) body) not supported
        })
    }

    /// Compile quote special form
    fn compile_quote(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() != 2 {
            return Err(ProcessedCompileError::new(
                "quote requires exactly 1 argument".to_string(),
            ));
        }

        let value = self.arena.alloc(self.compile_value(&elements[1])?);
        Ok(ProcessedValue::Quote { value })
    }

    /// Compile begin special form
    fn compile_begin(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() < 2 {
            return Ok(ProcessedValue::Unspecified);
        }

        let mut expressions = Vec::new();
        for expr in &elements[1..] {
            expressions.push(self.compile_value(expr)?);
        }

        let expr_slice = self.arena.alloc_slice_fill_iter(expressions.into_iter());
        Ok(ProcessedValue::Begin {
            expressions: Cow::Borrowed(expr_slice),
        })
    }

    /// Compile regular list (function application)
    fn compile_list(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        let mut compiled_elements = Vec::new();
        for element in elements {
            compiled_elements.push(self.compile_value(element)?);
        }

        let element_slice = self
            .arena
            .alloc_slice_fill_iter(compiled_elements.into_iter());
        Ok(ProcessedValue::List(Cow::Borrowed(element_slice)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_literals() {
        let ast = ProcessedAST::compile(&Value::Integer(42)).unwrap();
        match ast.root() {
            ProcessedValue::Integer(42) => (),
            _ => panic!("Expected integer 42"),
        }

        let ast = ProcessedAST::compile(&Value::Boolean(true)).unwrap();
        match ast.root() {
            ProcessedValue::Boolean(true) => (),
            _ => panic!("Expected boolean true"),
        }
    }

    #[test]
    fn test_compile_string_interning() {
        let ast = ProcessedAST::compile(&Value::String("hello".to_string())).unwrap();
        match ast.root() {
            ProcessedValue::String(sym) => {
                assert_eq!(ast.resolve_symbol(*sym), Some("hello"));
            }
            _ => panic!("Expected interned string"),
        }
    }

    #[test]
    fn test_compile_builtin_resolution() {
        let ast = ProcessedAST::compile(&Value::Symbol("+".to_string())).unwrap();
        match ast.root() {
            ProcessedValue::ResolvedBuiltin { name, .. } => {
                assert_eq!(ast.resolve_symbol(*name), Some("+"));
            }
            _ => panic!("Expected resolved builtin"),
        }
    }

    #[test]
    fn test_compile_if_form() {
        let if_expr = Value::List(vec![
            Value::Symbol("if".to_string()),
            Value::Boolean(true),
            Value::Integer(1),
            Value::Integer(2),
        ]);

        let ast = ProcessedAST::compile(&if_expr).unwrap();
        match ast.root() {
            ProcessedValue::If {
                test,
                then_branch,
                else_branch,
            } => {
                assert!(matches!(**test, ProcessedValue::Boolean(true)));
                assert!(matches!(**then_branch, ProcessedValue::Integer(1)));
                assert!(matches!(
                    else_branch.as_ref().unwrap(),
                    ProcessedValue::Integer(2)
                ));
            }
            _ => panic!("Expected If form"),
        }
    }

    #[test]
    fn test_compile_lambda_simple() {
        // Test: (lambda (x) (+ x 1))
        let lambda_expr = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol("x".to_string())]),
            Value::List(vec![
                Value::Symbol("+".to_string()),
                Value::Symbol("x".to_string()),
                Value::Integer(1),
            ]),
        ]);

        let ast = ProcessedAST::compile(&lambda_expr).unwrap();
        println!("Lambda AST dump:\n{}", ast.debug_dump());

        match ast.root() {
            ProcessedValue::Lambda {
                params,
                body,
                variadic,
            } => {
                assert_eq!(params.len(), 1);
                assert!(!variadic);

                let param_name = ast.resolve_symbol(params[0]).unwrap();
                assert_eq!(param_name, "x");

                // Body should be a list: (+ x 1)
                match body {
                    ProcessedValue::List(items) => {
                        assert_eq!(items.len(), 3);
                        // First should be resolved builtin +
                        assert!(matches!(items[0], ProcessedValue::ResolvedBuiltin { .. }));
                        // Second should be symbol x
                        assert!(matches!(items[1], ProcessedValue::Symbol(_)));
                        // Third should be integer 1
                        assert!(matches!(items[2], ProcessedValue::Integer(1)));
                    }
                    _ => panic!("Expected lambda body to be a list, got: {:?}", body),
                }
            }
            _ => panic!("Expected Lambda form, got: {:?}", ast.root()),
        }
    }

    #[test]
    fn test_compile_lambda_invocation() {
        // Test: ((lambda (x) (+ x 1)) 5)
        let invoke_expr = Value::List(vec![
            Value::List(vec![
                Value::Symbol("lambda".to_string()),
                Value::List(vec![Value::Symbol("x".to_string())]),
                Value::List(vec![
                    Value::Symbol("+".to_string()),
                    Value::Symbol("x".to_string()),
                    Value::Integer(1),
                ]),
            ]),
            Value::Integer(5),
        ]);

        let ast = ProcessedAST::compile(&invoke_expr).unwrap();
        println!("Lambda invocation AST dump:\n{}", ast.debug_dump());

        match ast.root() {
            ProcessedValue::List(items) => {
                assert_eq!(items.len(), 2);

                // First item should be the lambda
                match &items[0] {
                    ProcessedValue::Lambda {
                        params,
                        body,
                        variadic,
                    } => {
                        assert_eq!(params.len(), 1);
                        assert!(!variadic);

                        let param_name = ast.resolve_symbol(params[0]).unwrap();
                        assert_eq!(param_name, "x");

                        // Verify body structure
                        match body {
                            ProcessedValue::List(body_items) => {
                                assert_eq!(body_items.len(), 3);
                                assert!(matches!(
                                    body_items[0],
                                    ProcessedValue::ResolvedBuiltin { .. }
                                ));
                                assert!(matches!(body_items[1], ProcessedValue::Symbol(_)));
                                assert!(matches!(body_items[2], ProcessedValue::Integer(1)));
                            }
                            _ => panic!("Expected lambda body to be a list"),
                        }
                    }
                    _ => panic!("Expected first item to be Lambda, got: {:?}", &items[0]),
                }

                // Second item should be the argument
                assert!(matches!(items[1], ProcessedValue::Integer(5)));
            }
            _ => panic!("Expected List for lambda invocation, got: {:?}", ast.root()),
        }
    }
}
