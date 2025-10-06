// CPS-aware builtin functions
// Wrappers that adapt existing non-CPS builtins to CPS calling convention

use crate::value::Value;

// Note: CPS builtins no longer handle continuations directly.
// They return values normally, and the CPS-transformed calling code
// handles continuation calls through the VM's CallCont mechanism.

/// Identity function: returns its argument unchanged
pub fn identity_builtin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!("identity expects 1 argument, got {}", args.len()));
    }
    Ok(args[0].clone())
}

// Generate CPS wrappers for arithmetic operations
// For now, let's implement the CPS wrappers manually to avoid dependency issues
// We'll get the actual builtins from the HashMap at runtime

// We'll implement specific CPS wrappers that work with the builtin system

/// CPS-compatible addition - returns result directly
/// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
pub fn add_cps(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("add_cps expects at least 1 argument".to_string());
    }

    let mut sum = 0i64;
    for operand in args {
        match operand {
            Value::Integer(n) => {
                sum = sum
                    .checked_add(*n)
                    .ok_or_else(|| "Integer overflow in addition".to_string())?;
            }
            _ => {
                return Err(format!(
                    "Cannot add non-integer value: {}",
                    operand.type_name()
                ))
            }
        }
    }

    Ok(Value::Integer(sum))
}

/// CPS-compatible multiplication - returns result directly
/// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
pub fn mul_cps(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("mul_cps requires at least 1 argument".to_string());
    }

    let mut product = 1i64;
    for operand in args {
        match operand {
            Value::Integer(n) => {
                product = product
                    .checked_mul(*n)
                    .ok_or_else(|| "Integer overflow in multiplication".to_string())?;
            }
            _ => {
                return Err(format!(
                    "Cannot multiply non-integer value: {}",
                    operand.type_name()
                ))
            }
        }
    }

    Ok(Value::Integer(product))
}

/// CPS-compatible subtraction - returns result directly
/// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
pub fn sub_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("sub_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => x
            .checked_sub(*y)
            .ok_or_else(|| "Integer overflow in subtraction".to_string())?,
        _ => {
            return Err(format!(
                "Cannot subtract {} from {}: only integers supported",
                y.type_name(),
                x.type_name()
            ))
        }
    };

    Ok(Value::Integer(result))
}

/// CPS-compatible equality comparison - returns result directly
pub fn eq_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("eq_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    Ok(Value::Boolean(x == y))
}

/// CPS-compatible less-than comparison - returns result directly
pub fn lt_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("lt_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Boolean(x < y),
        _ => {
            return Err(format!(
                "Cannot compare {} with {}: only integers supported",
                x.type_name(),
                y.type_name()
            ))
        }
    };

    Ok(result)
}

// **R7RS RESTRICTED:** Division and modulo operations removed for simplicity
// Integer division behavior is surprising and error-prone

/// CPS-compatible greater-than comparison - returns result directly
pub fn gt_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("gt_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Boolean(x > y),
        _ => {
            return Err(format!(
                "Cannot compare {} with {}: only integers supported",
                x.type_name(),
                y.type_name()
            ))
        }
    };

    Ok(result)
}

/// CPS-compatible less-than-or-equal comparison - returns result directly
pub fn le_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("le_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Boolean(x <= y),
        _ => {
            return Err(format!(
                "Cannot compare {} with {}: only integers supported",
                x.type_name(),
                y.type_name()
            ))
        }
    };

    Ok(result)
}

/// CPS-compatible greater-than-or-equal comparison - returns result directly
pub fn ge_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("ge_cps requires exactly 2 arguments (x y)".to_string());
    }

    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Boolean(x >= y),
        _ => {
            return Err(format!(
                "Cannot compare {} with {}: only integers supported",
                x.type_name(),
                y.type_name()
            ))
        }
    };

    Ok(result)
}

/// CPS-compatible car (first element of list) - returns result directly
pub fn car_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("car_cps requires exactly 1 argument (lst)".to_string());
    }

    let lst = &args[0];

    let result = match lst {
        Value::List(ref elements) => {
            if elements.is_empty() {
                return Err("Cannot take car of empty list".to_string());
            }
            elements[0].clone()
        }
        _ => {
            return Err(format!("car requires a list, got {}", lst.type_name()));
        }
    };

    Ok(result)
}

/// CPS-compatible cdr (rest of list after first element) - returns result directly
pub fn cdr_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cdr_cps requires exactly 1 argument (lst)".to_string());
    }

    let lst = &args[0];

    let result = match lst {
        Value::List(ref elements) => {
            if elements.is_empty() {
                return Err("Cannot take cdr of empty list".to_string());
            }
            if elements.len() == 1 {
                Value::List(vec![])
            } else {
                Value::List(elements[1..].to_vec())
            }
        }
        _ => {
            return Err(format!("cdr requires a list, got {}", lst.type_name()));
        }
    };

    Ok(result)
}

/// CPS-compatible cons (construct list) - returns result directly
pub fn cons_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cons_cps requires exactly 2 arguments (elem lst)".to_string());
    }

    let elem = &args[0];
    let lst = &args[1];

    let result = match lst {
        Value::List(ref elements) => {
            let mut new_elements = vec![elem.clone()];
            new_elements.extend_from_slice(elements);
            Value::List(new_elements)
        }
        _ => {
            return Err(format!(
                "cons requires a list as second argument, got {}",
                lst.type_name()
            ));
        }
    };

    Ok(result)
}

/// CPS-compatible list construction - returns result directly
pub fn list_cps(args: &[Value]) -> Result<Value, String> {
    // All arguments become list elements - no continuation parameter
    let result = Value::List(args.to_vec());
    Ok(result)
}

/// CPS-compatible display (basic output) - returns unspecified directly
pub fn display_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("display_cps requires exactly 1 argument (value)".to_string());
    }

    let value = &args[0];

    // Display the value (in a real implementation, this would go to stdout)
    print!("{}", value);

    Ok(Value::Unspecified)
}

/// Type alias for CPS builtin function
type CPSBuiltinFn = fn(&[Value]) -> Result<Value, String>;

/// Get all CPS builtin functions
pub fn get_cps_builtins() -> Vec<(&'static str, CPSBuiltinFn)> {
    vec![
        // Identity continuation
        ("identity", identity_builtin),
        // Arithmetic operations - **R7RS RESTRICTED:** Division and modulo removed
        ("+", add_cps),
        ("*", mul_cps),
        ("-", sub_cps),
        // Comparison operations
        ("=", eq_cps),
        ("<", lt_cps),
        (">", gt_cps),
        ("<=", le_cps),
        (">=", ge_cps),
        // List operations
        ("car", car_cps),
        ("cdr", cdr_cps),
        ("cons", cons_cps),
        ("list", list_cps),
        // I/O operations
        ("display", display_cps),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity_builtin() {
        let result = identity_builtin(&[Value::Integer(42)]).unwrap();
        assert_eq!(result, Value::Integer(42));

        let result = identity_builtin(&[Value::Boolean(true)]).unwrap();
        assert_eq!(result, Value::Boolean(true));
    }

    #[test]
    fn test_identity_arity() {
        let result = identity_builtin(&[]);
        assert!(result.is_err());

        let result = identity_builtin(&[Value::Integer(1), Value::Integer(2)]);
        assert!(result.is_err());
    }

    #[test]
    fn test_add_cps_with_identity() {
        // Test: (+ 1 2 identity) should return 3
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = add_cps(&[Value::Integer(1), Value::Integer(2), identity]).unwrap();

        assert_eq!(result, Value::Integer(3));
    }

    #[test]
    fn test_cps_arithmetic_arity() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        // Too few arguments
        let result = add_cps(&[Value::Integer(1)]);
        assert!(result.is_err());

        // Just right
        let result = add_cps(&[Value::Integer(1), Value::Integer(2), identity.clone()]);
        assert!(result.is_ok());

        // More arguments (should work for +)
        let result = add_cps(&[
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            identity,
        ]);
        assert!(result.is_ok());
    }

    #[test]
    fn test_mul_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = mul_cps(&[Value::Integer(3), Value::Integer(4), identity]).unwrap();
        assert_eq!(result, Value::Integer(12));
    }

    #[test]
    fn test_sub_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = sub_cps(&[Value::Integer(10), Value::Integer(3), identity]).unwrap();
        assert_eq!(result, Value::Integer(7));
    }

    #[test]
    fn test_eq_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = eq_cps(&[Value::Integer(42), Value::Integer(42), identity.clone()]).unwrap();
        assert_eq!(result, Value::Boolean(true));

        let result = eq_cps(&[Value::Integer(42), Value::Integer(24), identity]).unwrap();
        assert_eq!(result, Value::Boolean(false));
    }

    #[test]
    fn test_lt_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = lt_cps(&[Value::Integer(3), Value::Integer(5), identity.clone()]).unwrap();
        assert_eq!(result, Value::Boolean(true));

        let result = lt_cps(&[Value::Integer(5), Value::Integer(3), identity]).unwrap();
        assert_eq!(result, Value::Boolean(false));
    }

    // **R7RS RESTRICTED:** Division and modulo tests removed

    #[test]
    fn test_gt_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = gt_cps(&[Value::Integer(5), Value::Integer(3), identity.clone()]).unwrap();
        assert_eq!(result, Value::Boolean(true));

        let result = gt_cps(&[Value::Integer(3), Value::Integer(5), identity]).unwrap();
        assert_eq!(result, Value::Boolean(false));
    }

    #[test]
    fn test_car_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let list = Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let result = car_cps(&[list, identity]).unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_cdr_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let list = Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let result = cdr_cps(&[list, identity]).unwrap();
        assert_eq!(
            result,
            Value::List(vec![Value::Integer(2), Value::Integer(3)])
        );
    }

    #[test]
    fn test_cons_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let list = Value::List(vec![Value::Integer(2), Value::Integer(3)]);
        let result = cons_cps(&[Value::Integer(1), list, identity]).unwrap();
        assert_eq!(
            result,
            Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])
        );
    }

    #[test]
    fn test_list_cps() {
        let identity = Value::Builtin {
            name: "identity".to_string(),
            arity: crate::value::Arity::Exact(1),
            func: identity_builtin,
        };

        let result = list_cps(&[
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            identity,
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3)
            ])
        );
    }
}
