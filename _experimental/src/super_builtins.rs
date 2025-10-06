// Super builtins for ProcessedAST evaluation
// These builtins work with ProcessedValue instead of Value, providing
// complete isolation from the legacy Value system in SuperVM

use crate::processed_env::ProcessedEnvironment;
use crate::vm::RuntimeError;
use std::borrow::Cow;
use std::rc::Rc;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

/// Type alias for string symbols
pub type StringSymbol = DefaultSymbol;

/// ProcessedValue enum - arena-allocated values for SuperVM
#[derive(Debug, Clone)]
pub enum ProcessedValue<'ast> {
    /// Boolean values (#t and #f)
    Boolean(bool),

    /// Signed exact integers
    Integer(i64),

    /// Unsigned exact integers  
    UInteger(u64),

    /// Inexact real numbers (f64)
    Real(f64),

    /// Interned string (for compiled/static data)
    String(StringSymbol),

    /// Owned string (for runtime-generated data)
    OwnedString(String),

    /// Interned symbol (for compiled/static data)
    Symbol(StringSymbol),

    /// Owned symbol (for runtime-generated data)
    OwnedSymbol(String),

    /// Proper lists with arena-allocated slices
    List(Cow<'ast, [ProcessedValue<'ast>]>),

    /// Built-in procedures with ProcessedValue signatures
    ResolvedBuiltin {
        name: StringSymbol,
        arity: ProcessedArity,
        func: for<'a> fn(&[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError>,
    },

    /// User-defined procedures (closures)
    Procedure {
        params: Cow<'ast, [StringSymbol]>,
        body: &'ast ProcessedValue<'ast>,
        env: Rc<ProcessedEnvironment<'ast>>,
        variadic: bool,
    },

    /// Special Forms - Direct ProcessedValue variants
    If {
        test: &'ast ProcessedValue<'ast>,
        then_branch: &'ast ProcessedValue<'ast>,
        else_branch: Option<&'ast ProcessedValue<'ast>>,
    },

    Define {
        name: StringSymbol,
        value: &'ast ProcessedValue<'ast>,
    },

    Lambda {
        params: Cow<'ast, [StringSymbol]>,
        body: &'ast ProcessedValue<'ast>,
        variadic: bool,
    },

    Quote {
        value: &'ast ProcessedValue<'ast>,
    },

    Begin {
        expressions: Cow<'ast, [ProcessedValue<'ast>]>,
    },

    /// letrec binding construct with parallel binding for mutual recursion
    Letrec {
        bindings: Cow<'ast, [(StringSymbol, ProcessedValue<'ast>)]>,
        body: &'ast ProcessedValue<'ast>,
    },

    /// Unspecified value (returned by some procedures)
    Unspecified,
}

/// Procedure arity specification for ProcessedValue
#[derive(Debug, Clone, Copy)]
pub enum ProcessedArity {
    /// Exactly n arguments
    Exact(usize),
    /// At least n arguments  
    AtLeast(usize),
    /// Between min and max arguments (inclusive)
    Range(usize, usize),
}

// ProcessedEnvironmentRef removed - using Rc<ProcessedEnvironment<'ast>> directly for consistency

impl<'ast> ProcessedValue<'ast> {
    /// Check if value is truthy (everything except #f is truthy in Scheme)
    pub fn is_truthy(&self) -> bool {
        !matches!(self, ProcessedValue::Boolean(false))
    }

    /// Get the type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            ProcessedValue::Boolean(_) => "boolean",
            ProcessedValue::Integer(_) => "integer",
            ProcessedValue::UInteger(_) => "unsigned-integer",
            ProcessedValue::Real(_) => "real",
            ProcessedValue::String(_) | ProcessedValue::OwnedString(_) => "string",
            ProcessedValue::Symbol(_) | ProcessedValue::OwnedSymbol(_) => "symbol",
            ProcessedValue::List(_) => "list",
            ProcessedValue::ResolvedBuiltin { .. } => "procedure",
            ProcessedValue::Procedure { .. } => "procedure",
            ProcessedValue::If { .. } => "if-expression",
            ProcessedValue::Define { .. } => "define-expression",
            ProcessedValue::Lambda { .. } => "lambda-expression",
            ProcessedValue::Quote { .. } => "quote-expression",
            ProcessedValue::Begin { .. } => "begin-expression",
            ProcessedValue::Letrec { .. } => "letrec-expression",
            ProcessedValue::Unspecified => "unspecified",
        }
    }

    /// Get string value from interner for display
    pub fn resolve_string<'a>(
        &'a self,
        interner: &'a StringInterner<DefaultBackend>,
    ) -> Option<&'a str> {
        match self {
            ProcessedValue::String(sym) | ProcessedValue::Symbol(sym) => interner.resolve(*sym),
            ProcessedValue::OwnedString(s) | ProcessedValue::OwnedSymbol(s) => Some(s.as_str()),
            _ => None,
        }
    }
}

impl<'ast> PartialEq for ProcessedValue<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ProcessedValue::Boolean(a), ProcessedValue::Boolean(b)) => a == b,
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a == b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a == b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a == b,
            (ProcessedValue::String(a), ProcessedValue::String(b)) => a == b,
            (ProcessedValue::Symbol(a), ProcessedValue::Symbol(b)) => a == b,
            (ProcessedValue::OwnedString(a), ProcessedValue::OwnedString(b)) => a == b,
            (ProcessedValue::OwnedSymbol(a), ProcessedValue::OwnedSymbol(b)) => a == b,
            // Cross-type string/symbol comparisons
            (ProcessedValue::String(_a), ProcessedValue::OwnedString(_b)) => {
                // Would need interner context to resolve - simplified for now
                false
            }
            (ProcessedValue::List(a), ProcessedValue::List(b)) => a == b,
            (ProcessedValue::Unspecified, ProcessedValue::Unspecified) => true,
            // Procedures are not comparable by value in Scheme
            _ => false,
        }
    }
}

/// Super builtin functions that work with ProcessedValue
pub mod builtin_functions {
    use super::*;

    /// Addition builtin for ProcessedValue
    pub fn add_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Ok(ProcessedValue::Integer(0));
        }

        let mut result = 0i64;
        let mut is_real = false;
        let mut real_result = 0.0f64;

        for arg in args {
            match arg {
                ProcessedValue::Integer(n) => {
                    if is_real {
                        real_result += *n as f64;
                    } else {
                        result += n;
                    }
                }
                ProcessedValue::UInteger(n) => {
                    if is_real {
                        real_result += *n as f64;
                    } else {
                        result += *n as i64;
                    }
                }
                ProcessedValue::Real(n) => {
                    if !is_real {
                        real_result = result as f64 + n;
                        is_real = true;
                    } else {
                        real_result += n;
                    }
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: + requires numbers, got {}",
                        arg.type_name()
                    )));
                }
            }
        }

        if is_real {
            Ok(ProcessedValue::Real(real_result))
        } else {
            Ok(ProcessedValue::Integer(result))
        }
    }

    /// Subtraction builtin for ProcessedValue
    pub fn sub_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Arity error: - requires at least 1 argument",
            ));
        }

        if args.len() == 1 {
            // Negation
            match &args[0] {
                ProcessedValue::Integer(n) => Ok(ProcessedValue::Integer(-n)),
                ProcessedValue::UInteger(n) => Ok(ProcessedValue::Integer(-(*n as i64))),
                ProcessedValue::Real(n) => Ok(ProcessedValue::Real(-n)),
                _ => Err(RuntimeError::new(format!(
                    "Type error: - requires numbers, got {}",
                    args[0].type_name()
                ))),
            }
        } else {
            // Subtraction
            let mut result = match &args[0] {
                ProcessedValue::Integer(n) => *n as f64,
                ProcessedValue::UInteger(n) => *n as f64,
                ProcessedValue::Real(n) => *n,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: - requires numbers, got {}",
                        args[0].type_name()
                    )));
                }
            };

            let mut is_real = matches!(&args[0], ProcessedValue::Real(_));

            for arg in &args[1..] {
                match arg {
                    ProcessedValue::Integer(n) => result -= *n as f64,
                    ProcessedValue::UInteger(n) => result -= *n as f64,
                    ProcessedValue::Real(n) => {
                        result -= n;
                        is_real = true;
                    }
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "Type error: - requires numbers, got {}",
                            arg.type_name()
                        )));
                    }
                }
            }

            if is_real || result.fract() != 0.0 {
                Ok(ProcessedValue::Real(result))
            } else {
                Ok(ProcessedValue::Integer(result as i64))
            }
        }
    }

    /// Multiplication builtin for ProcessedValue
    pub fn mul_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Ok(ProcessedValue::Integer(1));
        }

        let mut result = 1i64;
        let mut is_real = false;
        let mut real_result = 1.0f64;

        for arg in args {
            match arg {
                ProcessedValue::Integer(n) => {
                    if is_real {
                        real_result *= *n as f64;
                    } else {
                        result *= n;
                    }
                }
                ProcessedValue::UInteger(n) => {
                    if is_real {
                        real_result *= *n as f64;
                    } else {
                        result *= *n as i64;
                    }
                }
                ProcessedValue::Real(n) => {
                    if !is_real {
                        real_result = result as f64 * n;
                        is_real = true;
                    } else {
                        real_result *= n;
                    }
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: * requires numbers, got {}",
                        arg.type_name()
                    )));
                }
            }
        }

        if is_real {
            Ok(ProcessedValue::Real(real_result))
        } else {
            Ok(ProcessedValue::Integer(result))
        }
    }

    /// Equality builtin for ProcessedValue
    pub fn eq_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: = requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a == b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a == b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a == b,
            (ProcessedValue::Integer(a), ProcessedValue::Real(b)) => *a as f64 == *b,
            (ProcessedValue::Real(a), ProcessedValue::Integer(b)) => *a == *b as f64,
            (ProcessedValue::Boolean(a), ProcessedValue::Boolean(b)) => a == b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: = requires numbers or booleans, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Division builtin for ProcessedValue
    pub fn div_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Arity error: / requires at least 1 argument",
            ));
        }

        if args.len() == 1 {
            // (/ x) returns 1/x
            match &args[0] {
                ProcessedValue::Integer(n) => {
                    if *n == 0 {
                        return Err(RuntimeError::new("Division by zero".to_string()));
                    }
                    Ok(ProcessedValue::Real(1.0 / (*n as f64)))
                }
                ProcessedValue::UInteger(n) => {
                    if *n == 0 {
                        return Err(RuntimeError::new("Division by zero"));
                    }
                    Ok(ProcessedValue::Real(1.0 / (*n as f64)))
                }
                ProcessedValue::Real(n) => {
                    if *n == 0.0 {
                        return Err(RuntimeError::new("Division by zero"));
                    }
                    Ok(ProcessedValue::Real(1.0 / n))
                }
                _ => Err(RuntimeError::new(format!(
                    "Type error: / requires numbers, got {}",
                    args[0].type_name()
                ))),
            }
        } else {
            // (/ x y z ...) returns x / y / z / ...
            let mut result = match &args[0] {
                ProcessedValue::Integer(n) => *n as f64,
                ProcessedValue::UInteger(n) => *n as f64,
                ProcessedValue::Real(n) => *n,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: / requires numbers, got {}",
                        args[0].type_name()
                    )));
                }
            };

            for arg in &args[1..] {
                let divisor = match arg {
                    ProcessedValue::Integer(n) => *n as f64,
                    ProcessedValue::UInteger(n) => *n as f64,
                    ProcessedValue::Real(n) => *n,
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "Type error: / requires numbers, got {}",
                            arg.type_name()
                        )));
                    }
                };

                if divisor == 0.0 {
                    return Err(RuntimeError::new("Division by zero".to_string()));
                }
                result /= divisor;
            }

            // Check if result is a whole number
            if result.fract() == 0.0 && result.is_finite() {
                Ok(ProcessedValue::Integer(result as i64))
            } else {
                Ok(ProcessedValue::Real(result))
            }
        }
    }

    /// Less than builtin for ProcessedValue
    pub fn lt_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: < requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a < b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a < b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a < b,
            (ProcessedValue::Integer(a), ProcessedValue::Real(b)) => (*a as f64) < *b,
            (ProcessedValue::Real(a), ProcessedValue::Integer(b)) => *a < (*b as f64),
            (ProcessedValue::Integer(a), ProcessedValue::UInteger(b)) => (*a as u64) < *b,
            (ProcessedValue::UInteger(a), ProcessedValue::Integer(b)) => *a < (*b as u64),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: < requires numbers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Greater than builtin for ProcessedValue
    pub fn gt_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: > requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a > b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a > b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a > b,
            (ProcessedValue::Integer(a), ProcessedValue::Real(b)) => (*a as f64) > *b,
            (ProcessedValue::Real(a), ProcessedValue::Integer(b)) => *a > (*b as f64),
            (ProcessedValue::Integer(a), ProcessedValue::UInteger(b)) => (*a as u64) > *b,
            (ProcessedValue::UInteger(a), ProcessedValue::Integer(b)) => *a > (*b as u64),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: > requires numbers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Less than or equal builtin for ProcessedValue
    pub fn le_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: <= requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a <= b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a <= b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a <= b,
            (ProcessedValue::Integer(a), ProcessedValue::Real(b)) => (*a as f64) <= *b,
            (ProcessedValue::Real(a), ProcessedValue::Integer(b)) => *a <= (*b as f64),
            (ProcessedValue::Integer(a), ProcessedValue::UInteger(b)) => (*a as u64) <= *b,
            (ProcessedValue::UInteger(a), ProcessedValue::Integer(b)) => *a <= (*b as u64),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: <= requires numbers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Greater than or equal builtin for ProcessedValue
    pub fn ge_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: >= requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a >= b,
            (ProcessedValue::UInteger(a), ProcessedValue::UInteger(b)) => a >= b,
            (ProcessedValue::Real(a), ProcessedValue::Real(b)) => a >= b,
            (ProcessedValue::Integer(a), ProcessedValue::Real(b)) => (*a as f64) >= *b,
            (ProcessedValue::Real(a), ProcessedValue::Integer(b)) => *a >= (*b as f64),
            (ProcessedValue::Integer(a), ProcessedValue::UInteger(b)) => (*a as u64) >= *b,
            (ProcessedValue::UInteger(a), ProcessedValue::Integer(b)) => *a >= (*b as u64),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: >= requires numbers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Logical not builtin for ProcessedValue
    pub fn not_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: not requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        // **R7RS SEMANTICS:** Only #f is false, everything else is true
        let result = match &args[0] {
            ProcessedValue::Boolean(false) => true,
            _ => false,
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Car builtin for ProcessedValue (first element of list)
    pub fn car_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: car requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        match &args[0] {
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    Err(RuntimeError::new(
                        "Type error: car of empty list".to_string(),
                    ))
                } else {
                    Ok(elements[0].clone())
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Type error: car requires a list, got {}",
                args[0].type_name()
            ))),
        }
    }

    /// Cdr builtin for ProcessedValue (rest of list)
    pub fn cdr_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: cdr requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        match &args[0] {
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    Err(RuntimeError::new(
                        "Type error: cdr of empty list".to_string(),
                    ))
                } else {
                    // Return the rest of the list as a new list
                    let rest = elements[1..].to_vec();
                    Ok(ProcessedValue::List(Cow::Owned(rest)))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Type error: cdr requires a list, got {}",
                args[0].type_name()
            ))),
        }
    }

    /// Cons builtin for ProcessedValue (construct list)
    pub fn cons_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: cons requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        match &args[1] {
            ProcessedValue::List(elements) => {
                let mut new_list = vec![args[0].clone()];
                new_list.extend_from_slice(elements);
                Ok(ProcessedValue::List(Cow::Owned(new_list)))
            }
            _ => {
                // **R7RS DEVIATION:** Should create improper list, but we only support proper lists
                // For now, create a 2-element list
                Ok(ProcessedValue::List(Cow::Owned(vec![
                    args[0].clone(),
                    args[1].clone(),
                ])))
            }
        }
    }

    /// List builtin for ProcessedValue (construct list from arguments)
    pub fn list_super<'a>(args: &[ProcessedValue<'a>]) -> Result<ProcessedValue<'a>, RuntimeError> {
        // list can take any number of arguments (including zero)
        Ok(ProcessedValue::List(Cow::Owned(args.to_vec())))
    }

    // **R7RS RESTRICTED:** The following builtin functions are not implemented:
    // - Boolean operators: and, or (should be special forms, not functions)
    // - Type predicates: null?, pair?, number?, string?, symbol?, boolean?
    // - Additional list operations: length, append, reverse, memq, assq
    // - String operations: string-length, string-ref, string-set!, substring
    // - Vector operations: vector, vector-ref, vector-set!, vector-length
    // - I/O operations: read, write, display, newline
    // - Arithmetic: mod, abs, min, max, floor, ceiling, round, truncate
    // - Character operations: char=?, char<?, char>?, char->integer, integer->char
    // - Conversion functions: number->string, string->number, symbol->string, string->symbol
    //
    // **R7RS DEVIATION:** cons should support improper lists (dotted pairs) but currently
    // only creates proper lists when second argument is not a list.
    //
    // Current SuperVM builtin coverage:
    // ✅ Arithmetic: +, -, *, /, = (5/5 core operations)
    // ✅ Comparisons: <, >, <=, >= (4/4 core comparisons)
    // ✅ Logical: not (1/1 core logical function)
    // ✅ Lists: car, cdr, cons, list (4/6 core list operations)
    // ❌ Remaining: 50+ additional R7RS builtin functions not implemented
}

/// Create super builtins for ProcessedAST evaluation
pub fn create_super_builtins() -> Vec<(StringSymbol, ProcessedValue<'static>)> {
    // This will be implemented once we have the ProcessedAST structure
    // For now, return empty vec as placeholder
    vec![]
}
