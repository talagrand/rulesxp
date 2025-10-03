// Prolog injection system for implementing built-ins in Scheme
// This allows us to define many standard procedures in Scheme itself
// rather than implementing them all in Rust

/// Comprehensive Scheme prelude - procedures implemented in Scheme itself
/// These will be compiled and loaded before user code
pub const SCHEME_PRELUDE: &str = r#"
;; ===== LIST ACCESSOR COMBINATORS =====
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caar x) (car (car x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cadadr x) (car (cdr (car (cdr x)))))

;; ===== LIST UTILITIES =====
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(define (reverse lst)
  (define (reverse-helper lst acc)
    (if (null? lst)
        acc
        (reverse-helper (cdr lst) (cons (car lst) acc))))
  (reverse-helper lst '()))

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (list-tail lst n)
  (if (= n 0)
      lst
      (list-tail (cdr lst) (- n 1))))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;; ===== HIGHER-ORDER FUNCTIONS =====
(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (map proc (cdr lst)))))

(define (map2 proc lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (proc (car lst1) (car lst2)) 
            (map2 proc (cdr lst1) (cdr lst2)))))

(define (filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))

(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))

(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (fold-right proc init (cdr lst)))))

(define (for-each proc lst)
  (if (not (null? lst))
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

(define (any pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (any pred (cdr lst)))))

(define (every pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every pred (cdr lst))
          #f)))

;; ===== MATHEMATICAL OPERATIONS =====
(define (abs x)
  (if (< x 0) (- x) x))

(define (max x y)
  (if (> x y) x y))

(define (min x y)
  (if (< x y) x y))

(define (square x) (* x x))

(define (even? n) (= (modulo n 2) 0))
(define (odd? n) (not (even? n)))

(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (zero? x) (= x 0))

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

;; ===== LIST GENERATION =====
(define (make-list n obj)
  (if (= n 0)
      '()
      (cons obj (make-list (- n 1) obj))))

(define (iota n)
  (define (iota-helper i acc)
    (if (< i 0)
        acc
        (iota-helper (- i 1) (cons i acc))))
  (iota-helper (- n 1) '()))

(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

;; ===== BOOLEAN OPERATIONS =====
(define (not x) (if x #f #t))

;; ===== LIST PREDICATES =====
(define (member obj lst)
  (if (null? lst)
      #f
      (if (equal? obj (car lst))
          lst
          (member obj (cdr lst)))))

(define (assoc key alist)
  (if (null? alist)
      #f
      (if (equal? key (caar alist))
          (car alist)
          (assoc key (cdr alist)))))

;; ===== FUNCTION COMPOSITION =====
(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)

(define (const x)
  (lambda (y) x))

;; ===== CONTROL FLOW UTILITIES =====
(define (when test . body)
  (if test (begin . body)))

(define (unless test . body)
  (if (not test) (begin . body)))

;; ===== STRING UTILITIES (basic) =====
(define (string-length str)
  (length (string->list str)))

;; ===== DEBUGGING HELPERS =====
(define (trace msg val)
  (begin (display msg)
         (display ": ")
         (display val)
         (newline)
         val))

(define (assert condition msg)
  (if (not condition)
      (error "Assertion failed" msg)
      #t))

;; ===== COMBINATORS =====
(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

(define (uncurry f)
  (lambda (x y) ((f x) y)))

(define (flip f)
  (lambda (x y) (f y x)))

;; ===== ENVIRONMENT UTILITIES =====
(define (void) (if #f #f))

"#;

/// Inject the Scheme prelude before user code
/// This allows us to implement many built-ins in Scheme itself
pub fn inject_prelude(user_code: &str) -> String {
    format!("{}\n{}", SCHEME_PRELUDE, user_code)
}

/// Built-in procedures that must be implemented in Rust
/// These are the minimal set needed to bootstrap the Scheme prelude
pub fn create_builtins() -> std::collections::HashMap<String, crate::Value> {
    use crate::value::{Value, Arity};
    use std::collections::HashMap;
    
    let mut builtins = HashMap::new();
    
    // Arithmetic operations
    builtins.insert("+".to_string(), Value::Builtin {
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
    });
    
    builtins.insert("-".to_string(), Value::Builtin {
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
    });
    
    builtins.insert("*".to_string(), Value::Builtin {
        name: "*".to_string(),
        arity: Arity::AtLeast(0),
        func: |args| {
            let mut product = 1i64;
            for arg in args {
                match arg {
                    Value::Integer(n) => {
                        product = product.checked_mul(*n)
                            .ok_or("Integer overflow in multiplication")?;
                    }
                    Value::UInteger(n) => {
                        if *n > i64::MAX as u64 {
                            return Err("Integer overflow in multiplication".to_string());
                        }
                        product = product.checked_mul(*n as i64)
                            .ok_or("Integer overflow in multiplication")?;
                    }
                    _ => return Err(format!("* expects numbers, got {}", arg.type_name())),
                }
            }
            Ok(Value::Integer(product))
        },
    });
    
    // List operations - these are the core primitives needed
    builtins.insert("car".to_string(), Value::Builtin {
        name: "car".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::List(elements) => {
                    if elements.is_empty() {
                        Err("Cannot take car of empty list".to_string())
                    } else {
                        Ok(elements[0].clone())
                    }
                }
                _ => Err(format!("car expects list, got {}", args[0].type_name())),
            }
        },
    });
    
    builtins.insert("cdr".to_string(), Value::Builtin {
        name: "cdr".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::List(elements) => {
                    if elements.is_empty() {
                        Err("Cannot take cdr of empty list".to_string())
                    } else {
                        Ok(Value::List(elements[1..].to_vec()))
                    }
                }
                _ => Err(format!("cdr expects list, got {}", args[0].type_name())),
            }
        },
    });
    
    builtins.insert("cons".to_string(), Value::Builtin {
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
                _ => Err("cons expects list as second argument (improper lists not supported)".to_string()),
            }
        },
    });
    
    // Predicates
    builtins.insert("null?".to_string(), Value::Builtin {
        name: "null?".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::List(elements) => Ok(Value::Boolean(elements.is_empty())),
                _ => Ok(Value::Boolean(false)),
            }
        },
    });
    
    builtins.insert("eq?".to_string(), Value::Builtin {
        name: "eq?".to_string(),
        arity: Arity::Exact(2),
        func: |args| {
            Ok(Value::Boolean(args[0] == args[1]))
        },
    });
    
    // Type predicates
    builtins.insert("number?".to_string(), Value::Builtin {
        name: "number?".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::Integer(_) | Value::UInteger(_) | Value::Real(_) => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        },
    });
    
    builtins.insert("list?".to_string(), Value::Builtin {
        name: "list?".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::List(_) => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        },
    });
    
    // Display for debugging
    builtins.insert("display".to_string(), Value::Builtin {
        name: "display".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            print!("{}", args[0]);
            Ok(Value::Unspecified)
        },
    });
    
    builtins.insert("newline".to_string(), Value::Builtin {
        name: "newline".to_string(),
        arity: Arity::Exact(0),
        func: |_args| {
            println!();
            Ok(Value::Unspecified)
        },
    });
    
    // Additional arithmetic operations needed for prelude
    builtins.insert("modulo".to_string(), Value::Builtin {
        name: "modulo".to_string(),
        arity: Arity::Exact(2),
        func: |args| {
            match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => {
                    if *b == 0 {
                        return Err("Division by zero in modulo".to_string());
                    }
                    Ok(Value::Integer(a % b))
                }
                _ => Err("modulo expects integers".to_string()),
            }
        },
    });

    builtins.insert("/".to_string(), Value::Builtin {
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
    });

    // Equality and comparison
    builtins.insert("equal?".to_string(), Value::Builtin {
        name: "equal?".to_string(),
        arity: Arity::Exact(2),
        func: |args| {
            Ok(Value::Boolean(args[0] == args[1]))
        },
    });

    // String operations
    builtins.insert("string->list".to_string(), Value::Builtin {
        name: "string->list".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::String(s) => {
                    let chars: Vec<Value> = s.chars()
                        .map(|c| Value::String(c.to_string()))
                        .collect();
                    Ok(Value::List(chars))
                }
                _ => Err("string->list expects a string".to_string()),
            }
        },
    });

    builtins.insert("list->string".to_string(), Value::Builtin {
        name: "list->string".to_string(),
        arity: Arity::Exact(1),
        func: |args| {
            match &args[0] {
                Value::List(chars) => {
                    let mut result = String::new();
                    for char_val in chars {
                        match char_val {
                            Value::String(s) if s.len() == 1 => {
                                result.push_str(s);
                            }
                            _ => return Err("list->string expects list of single characters".to_string()),
                        }
                    }
                    Ok(Value::String(result))
                }
                _ => Err("list->string expects a list".to_string()),
            }
        },
    });

    // Error handling
    builtins.insert("error".to_string(), Value::Builtin {
        name: "error".to_string(),
        arity: Arity::AtLeast(1),
        func: |args| {
            let mut msg = format!("Error: {}", args[0]);
            for arg in &args[1..] {
                msg.push_str(&format!(" {}", arg));
            }
            Err(msg)
        },
    });

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
        
        let list = crate::Value::List(vec![crate::Value::Integer(1), crate::Value::Integer(2), crate::Value::Integer(3)]);
        let result = car_func(&[list]).unwrap();
        assert_eq!(result, crate::Value::Integer(1));
        
        // Test cons
        let cons_func = match &builtins["cons"] {
            crate::Value::Builtin { func, .. } => func,
            _ => panic!("Expected builtin function"),
        };
        
        let result = cons_func(&[
            crate::Value::Integer(0),
            crate::Value::List(vec![crate::Value::Integer(1), crate::Value::Integer(2)])
        ]).unwrap();
        assert_eq!(result, crate::Value::List(vec![crate::Value::Integer(0), crate::Value::Integer(1), crate::Value::Integer(2)]));
    }
}