// Built-in procedures module - implements core Scheme procedures in Rust
//
// ## R7RS Deviations and Limitations:
//
// **Missing Features:**
// - Many R7RS standard procedures not implemented yet
// - Character procedures (char?, char=?, etc.) - characters not supported
// - Vector procedures (vector?, vector-ref, etc.) - vectors not supported
// - Bytevector procedures - bytevectors not supported
// - Port/IO procedures - ports not supported
// - System procedures (command-line, environment variables, etc.)
//
// **Partial Implementations:**
// - Numeric procedures: Basic arithmetic only, missing advanced math functions
// - List procedures: Core functions implemented, missing some utilities
// - String procedures: Basic operations, missing advanced string manipulation
//
// **Architecture:**
// - Uses Scheme prelude system to implement many procedures in Scheme itself
// - Only core primitives that require Rust implementation are defined here

/// Combined function prelude including both standard functions and CPS functions
/// This provides all functions needed for both normal Scheme and CPS operations
const COMBINED_FUNCTION_PRELUDE: &str = concat!(
    include_str!("../prelude/functions.scm"),
    "\n",
    include_str!("../prelude/cps_functions.scm")
);

/// Inject the Scheme prelude before user code
/// This includes both standard functions and CPS functions
/// CPS macros are loaded automatically by MacroExpander::load_prelude()
pub fn inject_prelude(user_code: &str) -> String {
    format!("{}\n{}", COMBINED_FUNCTION_PRELUDE, user_code)
}

/// Built-in procedures that must be implemented in Rust
/// These are the minimal set needed to bootstrap the Scheme prelude
pub fn create_builtins() -> std::collections::HashMap<String, crate::Value> {
    use crate::value::{Arity, Value};
    use std::collections::HashMap;

    let mut builtins = HashMap::new();

    // Special builtin variables
    builtins.insert("$$-unspecified".to_string(), Value::Unspecified);

    // Arithmetic operations
    builtins.insert(
        "+".to_string(),
        Value::Builtin {
            name: "+".to_string(),
            arity: Arity::AtLeast(0),
            func: |args| {
                let mut sum = 0i64;
                for arg in args {
                    match arg {
                        Value::Integer(n) => sum += n,
                        Value::UInteger(n) => {
                            if *n > i64::MAX as u64 {
                                return Err("Integer overflow in addition".to_string());
                            }
                            sum += *n as i64;
                        }
                        _ => return Err(format!("+ expects numbers, got {}", arg.type_name())),
                    }
                }
                Ok(Value::Integer(sum))
            },
        },
    );

    builtins.insert(
        "-".to_string(),
        Value::Builtin {
            name: "-".to_string(),
            arity: Arity::AtLeast(1),
            func: |args| {
                if args.len() == 1 {
                    // Unary minus
                    match &args[0] {
                        Value::Integer(n) => Ok(Value::Integer(-n)),
                        Value::UInteger(n) => {
                            if *n > i64::MAX as u64 {
                                return Err("Integer overflow in negation".to_string());
                            }
                            Ok(Value::Integer(-(*n as i64)))
                        }
                        _ => Err(format!("- expects number, got {}", args[0].type_name())),
                    }
                } else {
                    // Subtraction
                    let mut result = match &args[0] {
                        Value::Integer(n) => *n,
                        Value::UInteger(n) => {
                            if *n > i64::MAX as u64 {
                                return Err("Integer overflow in subtraction".to_string());
                            }
                            *n as i64
                        }
                        _ => return Err(format!("- expects numbers, got {}", args[0].type_name())),
                    };

                    for arg in &args[1..] {
                        match arg {
                            Value::Integer(n) => result -= n,
                            Value::UInteger(n) => {
                                if *n > i64::MAX as u64 {
                                    return Err("Integer overflow in subtraction".to_string());
                                }
                                result -= *n as i64;
                            }
                            _ => return Err(format!("- expects numbers, got {}", arg.type_name())),
                        }
                    }
                    Ok(Value::Integer(result))
                }
            },
        },
    );

    builtins.insert(
        "*".to_string(),
        Value::Builtin {
            name: "*".to_string(),
            arity: Arity::AtLeast(0),
            func: |args| {
                let mut product = 1i64;
                for arg in args {
                    match arg {
                        Value::Integer(n) => {
                            product = product
                                .checked_mul(*n)
                                .ok_or("Integer overflow in multiplication")?;
                        }
                        Value::UInteger(n) => {
                            if *n > i64::MAX as u64 {
                                return Err("Integer overflow in multiplication".to_string());
                            }
                            product = product
                                .checked_mul(*n as i64)
                                .ok_or("Integer overflow in multiplication")?;
                        }
                        _ => return Err(format!("* expects numbers, got {}", arg.type_name())),
                    }
                }
                Ok(Value::Integer(product))
            },
        },
    );

    // List operations - these are the core primitives needed
    builtins.insert(
        "car".to_string(),
        Value::Builtin {
            name: "car".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::List(elements) => {
                    if elements.is_empty() {
                        Err("Cannot take car of empty list".to_string())
                    } else {
                        Ok(elements[0].clone())
                    }
                }
                _ => Err(format!("car expects list, got {}", args[0].type_name())),
            },
        },
    );

    builtins.insert(
        "cdr".to_string(),
        Value::Builtin {
            name: "cdr".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::List(elements) => {
                    if elements.is_empty() {
                        Err("Cannot take cdr of empty list".to_string())
                    } else {
                        Ok(Value::List(elements[1..].to_vec()))
                    }
                }
                _ => Err(format!("cdr expects list, got {}", args[0].type_name())),
            },
        },
    );

    builtins.insert(
        "cons".to_string(),
        Value::Builtin {
            name: "cons".to_string(),
            arity: Arity::Exact(2),
            func: |args| {
                let car = &args[0];
                match &args[1] {
                    Value::List(elements) => {
                        let mut new_list = vec![car.clone()];
                        new_list.extend_from_slice(elements);
                        Ok(Value::List(new_list))
                    }
                    _ => Err(
                        "cons expects list as second argument (improper lists not supported)"
                            .to_string(),
                    ),
                }
            },
        },
    );

    // List construction
    builtins.insert(
        "list".to_string(),
        Value::Builtin {
            name: "list".to_string(),
            arity: Arity::AtLeast(0),
            func: |args| Ok(Value::List(args.to_vec())),
        },
    );

    // Predicates
    builtins.insert(
        "null?".to_string(),
        Value::Builtin {
            name: "null?".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::List(elements) => Ok(Value::Boolean(elements.is_empty())),
                _ => Ok(Value::Boolean(false)),
            },
        },
    );

    builtins.insert(
        "eq?".to_string(),
        Value::Builtin {
            name: "eq?".to_string(),
            arity: Arity::Exact(2),
            func: |args| Ok(Value::Boolean(args[0] == args[1])),
        },
    );

    // Type predicates
    builtins.insert(
        "number?".to_string(),
        Value::Builtin {
            name: "number?".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::Integer(_) | Value::UInteger(_) | Value::Real(_) => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            },
        },
    );

    builtins.insert(
        "list?".to_string(),
        Value::Builtin {
            name: "list?".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::List(_) => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            },
        },
    );

    // Boolean operations - 'not' is a builtin to avoid ordering dependency with macro prelude
    // (macro prelude needs 'not' to be available when unless macro is defined)
    builtins.insert(
        "not".to_string(),
        Value::Builtin {
            name: "not".to_string(),
            arity: Arity::Exact(1),
            func: |args| {
                match &args[0] {
                    Value::Boolean(false) => Ok(Value::Boolean(true)),
                    _ => Ok(Value::Boolean(false)), // Everything else is truthy in Scheme
                }
            },
        },
    );

    // Display for debugging
    builtins.insert(
        "display".to_string(),
        Value::Builtin {
            name: "display".to_string(),
            arity: Arity::Exact(1),
            func: |args| {
                print!("{}", args[0]);
                Ok(Value::Unspecified)
            },
        },
    );

    builtins.insert(
        "newline".to_string(),
        Value::Builtin {
            name: "newline".to_string(),
            arity: Arity::Exact(0),
            func: |_args| {
                println!();
                Ok(Value::Unspecified)
            },
        },
    );

    // Additional arithmetic operations needed for prelude
    builtins.insert(
        "modulo".to_string(),
        Value::Builtin {
            name: "modulo".to_string(),
            arity: Arity::Exact(2),
            func: |args| match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => {
                    if *b == 0 {
                        return Err("Division by zero in modulo".to_string());
                    }
                    Ok(Value::Integer(a % b))
                }
                _ => Err("modulo expects integers".to_string()),
            },
        },
    );

    builtins.insert(
        "/".to_string(),
        Value::Builtin {
            name: "/".to_string(),
            arity: Arity::AtLeast(1),
            func: |args| {
                if args.is_empty() {
                    return Err("/ requires at least one argument".to_string());
                }

                let first = match &args[0] {
                    Value::Integer(n) => *n as f64,
                    Value::Real(n) => *n,
                    _ => return Err("/ expects numbers".to_string()),
                };

                if args.len() == 1 {
                    // (/ x) = 1/x
                    if first == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    return Ok(Value::Real(1.0 / first));
                }

                let mut result = first;
                for arg in &args[1..] {
                    let val = match arg {
                        Value::Integer(n) => *n as f64,
                        Value::Real(n) => *n,
                        _ => return Err("/ expects numbers".to_string()),
                    };
                    if val == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    result /= val;
                }

                // Return integer if result is whole
                if result.fract() == 0.0 && result.is_finite() {
                    Ok(Value::Integer(result as i64))
                } else {
                    Ok(Value::Real(result))
                }
            },
        },
    );

    // Equality and comparison
    builtins.insert(
        "equal?".to_string(),
        Value::Builtin {
            name: "equal?".to_string(),
            arity: Arity::Exact(2),
            func: |args| Ok(Value::Boolean(args[0] == args[1])),
        },
    );

    builtins.insert(
        "=".to_string(),
        Value::Builtin {
            name: "=".to_string(),
            arity: Arity::AtLeast(2),
            func: |args| {
                let first = &args[0];
                for arg in &args[1..] {
                    if first != arg {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            },
        },
    );

    builtins.insert(
        "<".to_string(),
        Value::Builtin {
            name: "<".to_string(),
            arity: Arity::AtLeast(2),
            func: |args| {
                for i in 0..args.len() - 1 {
                    let current = &args[i];
                    let next = &args[i + 1];
                    match (current, next) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            if a >= b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Real(b)) => {
                            if a >= b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Integer(a), Value::Real(b)) => {
                            if (*a as f64) >= *b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Integer(b)) => {
                            if *a >= (*b as f64) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        _ => return Err("< expects numbers".to_string()),
                    }
                }
                Ok(Value::Boolean(true))
            },
        },
    );

    builtins.insert(
        ">".to_string(),
        Value::Builtin {
            name: ">".to_string(),
            arity: Arity::AtLeast(2),
            func: |args| {
                for i in 0..args.len() - 1 {
                    let current = &args[i];
                    let next = &args[i + 1];
                    match (current, next) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            if a <= b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Real(b)) => {
                            if a <= b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Integer(a), Value::Real(b)) => {
                            if (*a as f64) <= *b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Integer(b)) => {
                            if *a <= (*b as f64) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        _ => return Err("> expects numbers".to_string()),
                    }
                }
                Ok(Value::Boolean(true))
            },
        },
    );

    builtins.insert(
        "<=".to_string(),
        Value::Builtin {
            name: "<=".to_string(),
            arity: Arity::AtLeast(2),
            func: |args| {
                for i in 0..args.len() - 1 {
                    let current = &args[i];
                    let next = &args[i + 1];
                    match (current, next) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            if a > b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Real(b)) => {
                            if a > b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Integer(a), Value::Real(b)) => {
                            if (*a as f64) > *b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Integer(b)) => {
                            if *a > (*b as f64) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        _ => return Err("<= expects numbers".to_string()),
                    }
                }
                Ok(Value::Boolean(true))
            },
        },
    );

    builtins.insert(
        ">=".to_string(),
        Value::Builtin {
            name: ">=".to_string(),
            arity: Arity::AtLeast(2),
            func: |args| {
                for i in 0..args.len() - 1 {
                    let current = &args[i];
                    let next = &args[i + 1];
                    match (current, next) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            if a < b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Real(b)) => {
                            if a < b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Integer(a), Value::Real(b)) => {
                            if (*a as f64) < *b {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        (Value::Real(a), Value::Integer(b)) => {
                            if *a < (*b as f64) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        _ => return Err(">= expects numbers".to_string()),
                    }
                }
                Ok(Value::Boolean(true))
            },
        },
    );

    // String operations
    builtins.insert(
        "string->list".to_string(),
        Value::Builtin {
            name: "string->list".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::String(s) => {
                    let chars: Vec<Value> =
                        s.chars().map(|c| Value::String(c.to_string())).collect();
                    Ok(Value::List(chars))
                }
                _ => Err("string->list expects a string".to_string()),
            },
        },
    );

    builtins.insert(
        "list->string".to_string(),
        Value::Builtin {
            name: "list->string".to_string(),
            arity: Arity::Exact(1),
            func: |args| match &args[0] {
                Value::List(chars) => {
                    let mut result = String::new();
                    for char_val in chars {
                        match char_val {
                            Value::String(s) if s.len() == 1 => {
                                result.push_str(s);
                            }
                            _ => {
                                return Err(
                                    "list->string expects list of single characters".to_string()
                                )
                            }
                        }
                    }
                    Ok(Value::String(result))
                }
                _ => Err("list->string expects a list".to_string()),
            },
        },
    );

    // Error handling
    builtins.insert(
        "error".to_string(),
        Value::Builtin {
            name: "error".to_string(),
            arity: Arity::AtLeast(1),
            func: |args| {
                let mut msg = format!("Error: {}", args[0]);
                for arg in &args[1..] {
                    msg.push_str(&format!(" {}", arg));
                }
                Err(msg)
            },
        },
    );

    builtins
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prelude_injection() {
        let user_code = "(+ 1 2)";
        let with_prelude = inject_prelude(user_code);

        assert!(with_prelude.contains("(define (cadr x)"));
        assert!(with_prelude.contains("(+ 1 2)"));
    }

    #[test]
    fn test_arithmetic_builtins() {
        let builtins = create_builtins();

        // Test addition
        let add_func = match &builtins["+"] {
            crate::Value::Builtin { func, .. } => func,
            _ => panic!("Expected builtin function"),
        };

        let result = add_func(&[crate::Value::Integer(1), crate::Value::Integer(2)]).unwrap();
        assert_eq!(result, crate::Value::Integer(3));

        // Test empty addition (should be 0)
        let result = add_func(&[]).unwrap();
        assert_eq!(result, crate::Value::Integer(0));
    }

    #[test]
    fn test_list_builtins() {
        let builtins = create_builtins();

        // Test car
        let car_func = match &builtins["car"] {
            crate::Value::Builtin { func, .. } => func,
            _ => panic!("Expected builtin function"),
        };

        let list = crate::Value::List(vec![
            crate::Value::Integer(1),
            crate::Value::Integer(2),
            crate::Value::Integer(3),
        ]);
        let result = car_func(&[list]).unwrap();
        assert_eq!(result, crate::Value::Integer(1));

        // Test cons
        let cons_func = match &builtins["cons"] {
            crate::Value::Builtin { func, .. } => func,
            _ => panic!("Expected builtin function"),
        };

        let result = cons_func(&[
            crate::Value::Integer(0),
            crate::Value::List(vec![crate::Value::Integer(1), crate::Value::Integer(2)]),
        ])
        .unwrap();
        assert_eq!(
            result,
            crate::Value::List(vec![
                crate::Value::Integer(0),
                crate::Value::Integer(1),
                crate::Value::Integer(2)
            ])
        );
    }
}
