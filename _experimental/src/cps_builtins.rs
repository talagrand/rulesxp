// CPS-aware builtin functions
// Wrappers that adapt existing non-CPS builtins to CPS calling convention

use crate::value::Value;

/// Call a continuation with given arguments
pub fn call_continuation(continuation: &Value, args: &[Value]) -> Result<Value, String> {
    match continuation {
        Value::Builtin { func, .. } => {
            // Builtin continuation (like identity)
            func(args)
        }
        Value::Procedure { .. } => {
            // User-defined continuation - need to call through VM
            // For now, return error - this will be handled by VM integration
            Err("User-defined continuations not yet supported in builtin wrappers".to_string())
        }
        _ => Err(format!(
            "Expected procedure as continuation, got {}",
            continuation.type_name()
        )),
    }
}

/// Identity continuation builtin: (lambda (x) x)
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

/// Simple add CPS wrapper for testing
pub fn add_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() < 3 {
        return Err(format!(
            "add_cps expects at least 3 arguments (operands + continuation), got {}",
            args.len()
        ));
    }

    let continuation = &args[args.len() - 1];
    let operands = &args[..args.len() - 1];

    // Simple addition logic
    let mut sum = 0i64;
    for operand in operands {
        match operand {
            Value::Integer(n) => sum += n,
            Value::UInteger(n) => sum += *n as i64,
            Value::Real(n) => sum += *n as i64,
            _ => {
                return Err(format!(
                    "Cannot add non-numeric value: {}",
                    operand.type_name()
                ))
            }
        }
    }

    let result = Value::Integer(sum);
    call_continuation(continuation, &[result])
}

/// CPS wrapper for multiplication
/// (mul_cps x y ... k) -> calls k with the product
pub fn mul_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 {
        return Err("mul_cps requires at least 2 arguments".to_string());
    }

    let continuation = &args[args.len() - 1];
    let operands = &args[..args.len() - 1];

    let mut product = 1i64;
    for operand in operands {
        match operand {
            Value::Integer(n) => product *= n,
            Value::UInteger(n) => product *= *n as i64,
            _ => {
                return Err(format!(
                    "Cannot multiply non-numeric value: {}",
                    operand.type_name()
                ))
            }
        }
    }

    let result = Value::Integer(product);
    call_continuation(continuation, &[result])
}

/// CPS wrapper for subtraction
/// (sub_cps x y k) -> calls k with (x - y)
pub fn sub_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("sub_cps requires exactly 3 arguments (x y k)".to_string());
    }

    let continuation = &args[2];
    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
        (Value::UInteger(x), Value::UInteger(y)) => {
            if *x >= *y {
                Value::UInteger(x - y)
            } else {
                Value::Integer(*x as i64 - *y as i64)
            }
        }
        (Value::Integer(x), Value::UInteger(y)) => Value::Integer(x - *y as i64),
        (Value::UInteger(x), Value::Integer(y)) => Value::Integer(*x as i64 - y),
        _ => {
            return Err(format!(
                "Cannot subtract {} from {}",
                y.type_name(),
                x.type_name()
            ))
        }
    };

    call_continuation(continuation, &[result])
}

/// CPS wrapper for equality comparison
/// (eq_cps x y k) -> calls k with #t if x == y, #f otherwise
pub fn eq_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("eq_cps requires exactly 3 arguments (x y k)".to_string());
    }

    let continuation = &args[2];
    let x = &args[0];
    let y = &args[1];

    let result = Value::Boolean(x == y);
    call_continuation(continuation, &[result])
}

/// CPS wrapper for less-than comparison
/// (lt_cps x y k) -> calls k with #t if x < y, #f otherwise
pub fn lt_cps(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("lt_cps requires exactly 3 arguments (x y k)".to_string());
    }

    let continuation = &args[2];
    let x = &args[0];
    let y = &args[1];

    let result = match (x, y) {
        (Value::Integer(x), Value::Integer(y)) => Value::Boolean(x < y),
        (Value::UInteger(x), Value::UInteger(y)) => Value::Boolean(x < y),
        (Value::Integer(x), Value::UInteger(y)) => Value::Boolean(*x < *y as i64),
        (Value::UInteger(x), Value::Integer(y)) => Value::Boolean((*x as i64) < *y),
        (Value::Real(x), Value::Real(y)) => Value::Boolean(x < y),
        _ => {
            return Err(format!(
                "Cannot compare {} with {}",
                x.type_name(),
                y.type_name()
            ))
        }
    };

    call_continuation(continuation, &[result])
}

/// Type alias for CPS builtin function
type CPSBuiltinFn = fn(&[Value]) -> Result<Value, String>;

/// Get all CPS builtin functions
pub fn get_cps_builtins() -> Vec<(&'static str, CPSBuiltinFn)> {
    vec![
        // Identity continuation
        ("identity", identity_builtin),
        // Arithmetic operations
        ("+", add_cps),
        ("*", mul_cps),
        ("-", sub_cps),
        // Comparison operations
        ("=", eq_cps),
        ("<", lt_cps),
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
}
