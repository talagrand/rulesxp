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
//! - Special forms (if, define, set!, lambda, quote, begin, letrec) → Specialized ProcessedValue variants
//!
//! ## R7RS Special Form Support
//!
//! **Implemented Special Forms** (compiled directly by ProcessedAST for SuperVM):
//! - `if` - Conditional expressions with optional else branch
//! - `define` - Variable and function definitions (creates mutable bindings)
//! - `set!` - Variable mutation (mutates existing bindings via Rc<RefCell<>>)
//! - `lambda` - Function creation with lexical scoping and environment capture
//! - `quote` - Literal data without evaluation
//! - `begin` - Sequential expression evaluation with proper last-value semantics
//! - `letrec` - Mutually recursive bindings via parallel initialization and back-patching
//!
//! **Derived Expressions** (handled by macro system, NOT ProcessedAST):
//! - `and`, `or` - Logical operators (macro-expanded to if expressions)
//! - `when`, `unless` - Simple conditionals (macro-expanded to if)
//! - `cond`, `case` - Multi-way conditionals (macro-expanded)
//! - `let`, `let*`, `do` - Local binding and iteration (macro-expanded)
//! - See `prelude/macros.scm` for complete macro definitions
//!
//! **Unsupported Special Forms** (R7RS RESTRICTED):
//! - `call/cc`, `call-with-current-continuation` - Continuations not implemented
//! - `dynamic-wind` - Dynamic extent control not implemented
//! - `let-syntax`, `letrec-syntax` - Local macro definitions not supported
//! - `define-syntax` - Should be handled by macro system before compilation
//!
//! **Lambda Restrictions** (R7RS RESTRICTED):
//! - Fully variadic form `(lambda args body)` supported
//! - Dot notation `(lambda (a b . rest) body)` NOT supported (returns error)
//!
//! **CRITICAL:** Input to ProcessedAST must be fully macro-expanded. The macro system
//! (see `macros.rs`) handles derived expressions before ProcessedAST compilation.
//!
//! ## Usage
//!
//! ## Single Expression Usage
//! ```rust
//! let ast = ProcessedAST::compile(&value)?;
//! let mut vm = SuperStackVM::new(ast);
//! let result = vm.evaluate(env)?;
//! ```
//!
//! ## Multiple Expression Usage
//! ```rust
//! let values = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
//! let ast = ProcessedAST::compile_multiple(&values)?;
//! let mut vm = SuperStackVM::new(ast);
//! let result = vm.evaluate(env)?;  // Returns result of last expression (3)
//! ```

use crate::super_builtins::{
    builtin_functions, ProcessedArity, ProcessedValue, SchemeStringInterner,
};
use crate::value::Value;
use bumpalo::Bump;
use std::borrow::Cow;
use std::collections::HashMap;
use string_interner::DefaultSymbol;

/// Type alias for string symbols
pub type StringSymbol = DefaultSymbol;

/// ProcessedAST with arena allocation and string interning
/// **NEEDS-FIX:** Using unsafe transmute for lifetime management - requires proper solution
pub struct ProcessedAST {
    /// String interner for symbols and strings
    pub interner: SchemeStringInterner,
    /// Bump arena for all allocations
    pub arena: Bump,
    /// Root expressions of the AST (multiple expressions can be compiled together)
    /// **UNSAFE:** Lifetime transmuted to 'static - arena must outlive all references
    pub root: Vec<ProcessedValue<'static>>,
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
    interner: &'arena mut SchemeStringInterner,
    arena: &'arena Bump,
    builtins: HashMap<StringSymbol, ProcessedValue<'arena>>,
}

impl ProcessedAST {
    /// Create a new ProcessedAST from a Value expression
    pub fn compile(value: &Value) -> Result<Self, ProcessedCompileError> {
        Self::compile_multiple(&[value.clone()])
    }

    /// Create a new ProcessedAST from multiple Value expressions
    pub fn compile_multiple(values: &[Value]) -> Result<Self, ProcessedCompileError> {
        let mut interner = SchemeStringInterner::new();
        let arena = Bump::new();

        // Create builtin function mappings directly (avoiding borrowing issues)
        let mut builtins = HashMap::new();
        Self::populate_builtins(&mut builtins, &mut interner);

        let mut compiler = ProcessedCompiler {
            interner: &mut interner,
            arena: &arena,
            builtins,
        };

        // Compile all values into the root vector
        let mut compiled_roots = Vec::new();
        for value in values {
            let compiled = compiler.compile_value(value)?;
            // Due to lifetime constraints, we need to use unsafe here
            // This is safe because the arena owns all the data and the ProcessedAST
            // owns the arena, so the lifetimes are actually correct
            let compiled_static = unsafe {
                std::mem::transmute::<ProcessedValue<'_>, ProcessedValue<'static>>(compiled)
            };
            compiled_roots.push(compiled_static);
        }

        Ok(ProcessedAST {
            interner,
            arena,
            root: compiled_roots,
        })
    }

    /// Populate the builtin function mappings for the compiler
    /// This is shared between compile and compile_multiple
    fn populate_builtins(
        builtins: &mut HashMap<StringSymbol, ProcessedValue<'_>>,
        interner: &mut SchemeStringInterner,
    ) {
        // Add arithmetic builtins
        let add_sym = interner.get_or_intern("+");
        let sub_sym = interner.get_or_intern("-");
        let mul_sym = interner.get_or_intern("*");
        let _div_sym = interner.get_or_intern("/");
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
        let null_sym = interner.get_or_intern("null?");
        let display_sym = interner.get_or_intern("display");
        let error_sym = interner.get_or_intern("error");

        builtins.insert(
            add_sym,
            ProcessedValue::ResolvedBuiltin {
                name: add_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::builtin_add,
            },
        );

        builtins.insert(
            sub_sym,
            ProcessedValue::ResolvedBuiltin {
                name: sub_sym,
                arity: ProcessedArity::AtLeast(1),
                func: builtin_functions::builtin_sub,
            },
        );

        builtins.insert(
            mul_sym,
            ProcessedValue::ResolvedBuiltin {
                name: mul_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::builtin_mul,
            },
        );

        builtins.insert(
            eq_sym,
            ProcessedValue::ResolvedBuiltin {
                name: eq_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_eq,
            },
        );

        // **R7RS RESTRICTED:** Division builtin removed for simplicity

        builtins.insert(
            lt_sym,
            ProcessedValue::ResolvedBuiltin {
                name: lt_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_lt,
            },
        );

        builtins.insert(
            gt_sym,
            ProcessedValue::ResolvedBuiltin {
                name: gt_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_gt,
            },
        );

        builtins.insert(
            le_sym,
            ProcessedValue::ResolvedBuiltin {
                name: le_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_le,
            },
        );

        builtins.insert(
            ge_sym,
            ProcessedValue::ResolvedBuiltin {
                name: ge_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_ge,
            },
        );

        builtins.insert(
            not_sym,
            ProcessedValue::ResolvedBuiltin {
                name: not_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::builtin_not,
            },
        );

        builtins.insert(
            car_sym,
            ProcessedValue::ResolvedBuiltin {
                name: car_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::builtin_car,
            },
        );

        builtins.insert(
            cdr_sym,
            ProcessedValue::ResolvedBuiltin {
                name: cdr_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::builtin_cdr,
            },
        );

        builtins.insert(
            cons_sym,
            ProcessedValue::ResolvedBuiltin {
                name: cons_sym,
                arity: ProcessedArity::Exact(2),
                func: builtin_functions::builtin_cons,
            },
        );

        builtins.insert(
            list_sym,
            ProcessedValue::ResolvedBuiltin {
                name: list_sym,
                arity: ProcessedArity::AtLeast(0),
                func: builtin_functions::builtin_list,
            },
        );

        builtins.insert(
            display_sym,
            ProcessedValue::ResolvedBuiltin {
                name: display_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::builtin_display,
            },
        );

        builtins.insert(
            null_sym,
            ProcessedValue::ResolvedBuiltin {
                name: null_sym,
                arity: ProcessedArity::Exact(1),
                func: builtin_functions::builtin_null,
            },
        );

        builtins.insert(
            error_sym,
            ProcessedValue::ResolvedBuiltin {
                name: error_sym,
                arity: ProcessedArity::AtLeast(1),
                func: builtin_functions::builtin_error,
            },
        );
    }

    /// Get the root expressions with proper lifetime
    pub fn root(&self) -> &Vec<ProcessedValue<'static>> {
        &self.root
    }

    /// Resolve a string symbol to its string value
    pub fn resolve_symbol(&self, symbol: StringSymbol) -> Option<&str> {
        self.interner.resolve(symbol)
    }

    /// Debug dump the ProcessedAST tree structure
    pub fn debug_dump(&self) -> String {
        if self.root.is_empty() {
            return "Empty AST".to_string();
        }

        let mut result = String::new();
        for (i, expr) in self.root.iter().enumerate() {
            if i > 0 {
                result.push_str("\n---\n");
            }
            result.push_str(&self.dump_value(expr, 0));
        }
        result
    }

    /// Recursively dump a ProcessedValue with indentation
    fn dump_value(&self, value: &ProcessedValue, indent: usize) -> String {
        let prefix = "  ".repeat(indent);
        match value {
            ProcessedValue::Boolean(b) => format!("{}Boolean({})", prefix, b),
            ProcessedValue::Integer(n) => format!("{}Integer({})", prefix, n),
            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats
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
            ProcessedValue::Set { name, value } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                let mut result = format!("{}Set[{}]\n", prefix, name_str);
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
            ProcessedValue::Letrec { bindings, body } => {
                let mut result = format!("{}Letrec[\n", prefix);
                result.push_str(&format!("{}  bindings:\n", prefix));
                for (name, init) in bindings.iter() {
                    let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                    result.push_str(&format!("{}    {} =>\n", prefix, name_str));
                    result.push_str(&self.dump_value(init, indent + 3));
                    result.push('\n');
                }
                result.push_str(&format!("{}  body:\n", prefix));
                result.push_str(&self.dump_value(body, indent + 2));
                result.push('\n');
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
            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats

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
                        "set!" => self.compile_set(elements),
                        "lambda" => self.compile_lambda(elements),
                        "quote" => self.compile_quote(elements),
                        "begin" => self.compile_begin(elements),
                        "letrec" => self.compile_letrec(elements),
                        // **R7RS DEVIATION:** The following standard R7RS special forms are NOT implemented in ProcessedAST
                        // These forms are handled by the macro system instead (see prelude/macros.scm):
                        "and" | "or" | "when" | "unless" | "cond" | "case" | "let" | "let*" | "do" => {
                            Err(ProcessedCompileError::new(format!(
                                "R7RS DEVIATION: {} is a derived expression handled by macro system, not ProcessedAST compiler. \
                                 Must be macro-expanded before ProcessedAST compilation.", first
                            )))
                        }
                        // **R7RS RESTRICTED:** The following forms are not supported at all:
                        "call/cc" | "call-with-current-continuation" => {
                            Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: Continuations (call/cc) not implemented in ProcessedAST".to_string()
                            ))
                        }
                        "dynamic-wind" => {
                            Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: dynamic-wind not implemented in ProcessedAST".to_string()
                            ))
                        }
                        "let-syntax" | "letrec-syntax" => {
                            Err(ProcessedCompileError::new(
                                "R7RS RESTRICTED: Local macro definitions not supported in ProcessedAST".to_string()
                            ))
                        }
                        "define-syntax" => {
                            Err(ProcessedCompileError::new(
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
        if elements.len() < 3 {
            return Err(ProcessedCompileError::new(
                "define requires at least 2 arguments".to_string(),
            ));
        }

        let (name, value_expr) = match &elements[1] {
            // Variable definition: (define var expr)
            Value::Symbol(s) => {
                if elements.len() != 3 {
                    return Err(ProcessedCompileError::new(
                        "Variable define requires exactly 1 value expression".to_string(),
                    ));
                }
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

                        // Handle multiple body expressions by wrapping in begin
                        let body = if elements.len() == 3 {
                            // Single body expression
                            elements[2].clone()
                        } else {
                            // Multiple body expressions - wrap in begin
                            let mut begin_elements = vec![Value::Symbol("begin".to_string())];
                            begin_elements.extend(elements[2..].iter().cloned());
                            Value::List(begin_elements)
                        };

                        // Compile lambda directly without creating temporary Value
                        // This is equivalent to: (lambda (args...) body...)
                        let lambda_elements = vec![
                            Value::Symbol("lambda".to_string()),
                            Value::List(params),
                            body,
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

    /// Compile set! special form
    ///
    /// **R7RS 5.3.2:** set! mutates an existing binding.
    /// The variable must already be defined, otherwise an error is returned.
    /// All define'd variables and lambda parameters are mutable (Rc<RefCell<>>).
    fn compile_set(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() != 3 {
            return Err(ProcessedCompileError::new(format!(
                "set! requires exactly 2 arguments (variable and value), got {}",
                elements.len() - 1
            )));
        }

        let var = &elements[1];
        let value_expr = &elements[2];

        // Variable must be a symbol
        let name_symbol = match var {
            Value::Symbol(s) => self.interner.get(s).ok_or_else(|| {
                ProcessedCompileError::new(format!("Symbol not found in interner: {}", s))
            })?,
            _ => {
                return Err(ProcessedCompileError::new(
                    "set! requires a symbol as first argument".to_string(),
                ))
            }
        };

        let value = self.arena.alloc(self.compile_value(value_expr)?);
        Ok(ProcessedValue::Set {
            name: name_symbol,
            value,
        })
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

        let (params, variadic) = match &elements[1] {
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
                (param_symbols, false) // List of parameters = fixed arity
            }
            Value::Symbol(single_param) => {
                // Single symbol = fully variadic
                (vec![self.interner.get_or_intern(single_param)], true)
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
            let begin_slice = self.arena.alloc_slice_fill_iter(begin_exprs);
            self.arena.alloc(ProcessedValue::Begin {
                expressions: Cow::Borrowed(begin_slice),
            })
        };

        let params_slice = self.arena.alloc_slice_fill_iter(params);

        Ok(ProcessedValue::Lambda {
            params: Cow::Borrowed(params_slice),
            body,
            variadic, // **R7RS RESTRICTED:** Only fully variadic functions (lambda args body) supported, dot notation not supported
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

        let expr_slice = self.arena.alloc_slice_fill_iter(expressions);
        Ok(ProcessedValue::Begin {
            expressions: Cow::Borrowed(expr_slice),
        })
    }

    /// Compile letrec special form
    fn compile_letrec(
        &mut self,
        elements: &[Value],
    ) -> Result<ProcessedValue<'arena>, ProcessedCompileError> {
        if elements.len() < 3 {
            return Err(ProcessedCompileError::new(
                "letrec requires at least 2 arguments: bindings and body".to_string(),
            ));
        }

        // Parse bindings list
        let bindings_list = match &elements[1] {
            Value::List(bindings) => bindings,
            _ => {
                return Err(ProcessedCompileError::new(
                    "letrec bindings must be a list".to_string(),
                ))
            }
        };

        // Zero bindings is an error (R7RS)
        if bindings_list.is_empty() {
            return Err(ProcessedCompileError::new(
                "letrec requires at least one binding".to_string(),
            ));
        }

        let mut compiled_bindings = Vec::new();
        let mut seen_names = std::collections::HashSet::new();
        for binding in bindings_list {
            match binding {
                Value::List(binding_pair) if binding_pair.len() == 2 => {
                    let name = match &binding_pair[0] {
                        Value::Symbol(name) => self.interner.get_or_intern(name),
                        _ => {
                            return Err(ProcessedCompileError::new(
                                "letrec binding variable must be a symbol".to_string(),
                            ))
                        }
                    };
                    if !seen_names.insert(name) {
                        return Err(ProcessedCompileError::new(
                            "Duplicate variable in letrec".to_string(),
                        ));
                    }
                    let init = self.compile_value(&binding_pair[1])?;
                    compiled_bindings.push((name, init));
                }
                _ => {
                    return Err(ProcessedCompileError::new(
                        "letrec binding must be a list of (variable init)".to_string(),
                    ))
                }
            }
        }

        let bindings_slice = self.arena.alloc_slice_fill_iter(compiled_bindings);

        // **R7RS COMPLIANCE:** Support multiple body expressions by wrapping in implicit begin
        let body = if elements.len() == 3 {
            // Single body expression - use directly
            self.arena.alloc(self.compile_value(&elements[2])?)
        } else {
            // Multiple body expressions - wrap in begin
            let mut body_exprs = Vec::new();
            for expr in &elements[2..] {
                body_exprs.push(self.compile_value(expr)?);
            }
            let body_slice = self.arena.alloc_slice_fill_iter(body_exprs);
            self.arena.alloc(ProcessedValue::Begin {
                expressions: Cow::Borrowed(body_slice),
            })
        };

        Ok(ProcessedValue::Letrec {
            bindings: Cow::Borrowed(bindings_slice),
            body,
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

        let element_slice = self.arena.alloc_slice_fill_iter(compiled_elements);
        Ok(ProcessedValue::List(Cow::Borrowed(element_slice)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_literals() {
        let ast = ProcessedAST::compile(&Value::Integer(42)).unwrap();
        match &ast.root()[0] {
            ProcessedValue::Integer(42) => (),
            _ => panic!("Expected integer 42"),
        }

        let ast = ProcessedAST::compile(&Value::Boolean(true)).unwrap();
        match &ast.root()[0] {
            ProcessedValue::Boolean(true) => (),
            _ => panic!("Expected boolean true"),
        }
    }

    #[test]
    fn test_compile_string_interning() {
        let ast = ProcessedAST::compile(&Value::String("hello".to_string())).unwrap();
        match &ast.root()[0] {
            ProcessedValue::String(sym) => {
                assert_eq!(ast.resolve_symbol(*sym), Some("hello"));
            }
            _ => panic!("Expected interned string"),
        }
    }

    #[test]
    fn test_compile_builtin_resolution() {
        let ast = ProcessedAST::compile(&Value::Symbol("+".to_string())).unwrap();
        match &ast.root()[0] {
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
        match &ast.root()[0] {
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

        match &ast.root()[0] {
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

        match &ast.root()[0] {
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

    #[test]
    fn test_compile_multiple_expressions() {
        // Test compiling multiple expressions
        let values = vec![
            Value::Integer(42),
            Value::Boolean(true),
            Value::Symbol("+".to_string()),
            Value::List(vec![
                Value::Symbol("+".to_string()),
                Value::Integer(1),
                Value::Integer(2),
            ]),
        ];

        let ast = ProcessedAST::compile_multiple(&values).unwrap();

        // Should have 4 expressions in the root
        assert_eq!(ast.root().len(), 4);

        // First should be integer 42
        match &ast.root()[0] {
            ProcessedValue::Integer(42) => (),
            _ => panic!("Expected first expression to be integer 42"),
        }

        // Second should be boolean true
        match &ast.root()[1] {
            ProcessedValue::Boolean(true) => (),
            _ => panic!("Expected second expression to be boolean true"),
        }

        // Third should be resolved builtin +
        match &ast.root()[2] {
            ProcessedValue::ResolvedBuiltin { name, .. } => {
                assert_eq!(ast.resolve_symbol(*name), Some("+"));
            }
            _ => panic!("Expected third expression to be resolved builtin +"),
        }

        // Fourth should be a list (+ 1 2)
        match &ast.root()[3] {
            ProcessedValue::List(items) => {
                assert_eq!(items.len(), 3);
                assert!(matches!(items[0], ProcessedValue::ResolvedBuiltin { .. }));
                assert!(matches!(items[1], ProcessedValue::Integer(1)));
                assert!(matches!(items[2], ProcessedValue::Integer(2)));
            }
            _ => panic!("Expected fourth expression to be a list"),
        }
    }

    #[test]
    fn test_compile_multiple_empty() {
        // Test compiling empty array
        let values: Vec<Value> = vec![];
        let ast = ProcessedAST::compile_multiple(&values).unwrap();

        // Should have 0 expressions in the root
        assert_eq!(ast.root().len(), 0);

        // Debug dump should show empty AST
        assert_eq!(ast.debug_dump(), "Empty AST");
    }
}
