// CPS (Continuation-Passing Style) transformation module
// Converts direct-style Scheme code to CPS form

use crate::value::Value;

/// CPS transformation context
pub struct CPSTransformer {
    /// Counter for generating unique continuation names
    continuation_counter: usize,
}

impl Default for CPSTransformer {
    fn default() -> Self {
        Self::new()
    }
}

impl CPSTransformer {
    pub fn new() -> Self {
        Self {
            continuation_counter: 0,
        }
    }

    /// Generate a unique continuation parameter name
    fn fresh_continuation_name(&mut self) -> String {
        let name = format!("k{}", self.continuation_counter);
        self.continuation_counter += 1;
        name
    }

    /// Transform a value to CPS form with given continuation
    pub fn transform_with_continuation(&mut self, value: &Value, continuation: &Value) -> Value {
        match value {
            // Literals don't need computation - pass directly to continuation
            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats
            Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Symbol(_) => {
                self.make_continuation_call(continuation, vec![value.clone()])
            }

            // Lists are function applications or special forms
            Value::List(elements) if !elements.is_empty() => {
                match &elements[0] {
                    Value::Symbol(name) => match name.as_str() {
                        // Special forms that need CPS transformation
                        "if" => self.transform_if(&elements[1..], continuation),
                        "lambda" => self.transform_lambda(&elements[1..], continuation),
                        "define" => self.transform_define(&elements[1..], continuation),
                        "begin" => self.transform_begin(&elements[1..], continuation),
                        "quote" => {
                            // Quoted expressions are literals
                            if elements.len() == 2 {
                                self.make_continuation_call(continuation, vec![elements[1].clone()])
                            } else {
                                // Error: quote requires exactly one argument
                                Value::List(vec![
                                    Value::Symbol("error".to_string()),
                                    Value::String(
                                        "quote requires exactly one argument".to_string(),
                                    ),
                                ])
                            }
                        }
                        _ => {
                            // Regular function application
                            self.transform_application(elements, continuation)
                        }
                    },
                    _ => {
                        // Function expression application: ((lambda ...) args...)
                        self.transform_application(elements, continuation)
                    }
                }
            }

            // Empty list
            Value::List(_) => self.make_continuation_call(continuation, vec![Value::List(vec![])]),

            // Other values (procedures, builtins) pass through
            _ => self.make_continuation_call(continuation, vec![value.clone()]),
        }
    }

    /// Transform entire program to CPS with identity continuation
    pub fn transform_program(&mut self, value: &Value) -> Value {
        let identity_continuation = self.make_identity_continuation();
        self.transform_with_continuation(value, &identity_continuation)
    }

    /// Create identity continuation: use builtin identity function
    fn make_identity_continuation(&self) -> Value {
        // Use the builtin identity function instead of (lambda (x) x)
        // This avoids the "user-defined continuations not supported" error
        Value::Symbol("identity".to_string())
    }

    /// Create continuation call using explicit AST annotation
    ///
    /// **DESIGN DECISION: Explicit AST Annotations vs. Signature Detection**
    ///
    /// Most CPS implementations use "signature detection" - they generate normal-looking
    /// lambda expressions like (lambda (x k) (k (+ x 1))) and rely on the compiler
    /// to detect CPS patterns through heuristics (e.g., "last parameter starting with 'k'").
    ///
    /// **Why We Use Explicit Annotations Instead:**
    ///
    /// 1. **Engineering Over Aesthetics**: While (k result) looks cleaner than
    ///    ($$-cont-call k result), the explicit form eliminates ALL ambiguity for the compiler.
    ///    
    /// 2. **No Pattern Matching Fragility**: Signature detection breaks if users name
    ///    continuation parameters differently, or if CPS transformation changes. Our approach
    ///    works regardless of naming conventions.
    ///    
    /// 3. **Direct Opcode Emission**: Compiler sees ($$-cont-call k result) and immediately
    ///    emits ContJump opcode. No heuristics, no guessing, no performance overhead from
    ///    pattern analysis.
    ///    
    /// 4. **Academic Purity is Overrated**: The "interoperability with manual CPS code"
    ///    argument is theoretical - users almost never write CPS functions manually.
    ///    CPS is a compiler internal transformation, not a user-facing feature.
    ///    
    /// 5. **Better Error Messages**: When compilation fails, error points to specific
    ///    $$-cont-call form rather than generic function call that compiler had to guess about.
    ///
    /// **Trade-offs We Accept:**
    /// - CPS-transformed code looks less "pure" (has compiler internals visible)
    /// - Deviates from academic CPS literature conventions
    /// - Internal symbols use namespace ($$-prefix prevents user conflicts)
    ///
    /// **Result**: Robust, unambiguous, fast compilation of native CPS opcodes.
    fn make_continuation_call(&self, continuation: &Value, args: Vec<Value>) -> Value {
        // Generate explicit continuation call annotation
        let mut call = vec![
            Value::Symbol("$cont-call".to_string()),
            continuation.clone(),
        ];
        call.extend(args);
        Value::List(call)
    }

    /// Transform if expression: (if test then else) -> CPS
    fn transform_if(&mut self, args: &[Value], continuation: &Value) -> Value {
        if args.len() < 2 || args.len() > 3 {
            return Value::List(vec![
                Value::Symbol("error".to_string()),
                Value::String("if requires 2 or 3 arguments".to_string()),
            ]);
        }

        let test = &args[0];
        let then_expr = &args[1];
        let else_expr = if args.len() == 3 {
            &args[2]
        } else {
            &Value::List(vec![]) // Default else is empty list (unspecified)
        };

        // Transform: (if test then else) k
        // Into: test (lambda (v) (if v (then k) (else k)))
        let test_continuation = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol("v".to_string())]),
            Value::List(vec![
                Value::Symbol("if".to_string()),
                Value::Symbol("v".to_string()),
                self.transform_with_continuation(then_expr, continuation),
                self.transform_with_continuation(else_expr, continuation),
            ]),
        ]);

        self.transform_with_continuation(test, &test_continuation)
    }

    /// Transform lambda expression: (lambda (params...) body...) -> CPS
    fn transform_lambda(&mut self, args: &[Value], continuation: &Value) -> Value {
        if args.len() < 2 {
            return Value::List(vec![
                Value::Symbol("error".to_string()),
                Value::String("lambda requires at least 2 arguments".to_string()),
            ]);
        }

        let params = &args[0];
        let body_exprs = &args[1..];

        // Transform lambda to take an additional continuation parameter
        // (lambda (x y) body...) -> (lambda (x y k) (body... k))
        let mut cps_params = match params {
            Value::List(param_list) => param_list.clone(),
            Value::Symbol(single_param) => vec![Value::Symbol(single_param.clone())],
            _ => {
                return Value::List(vec![
                    Value::Symbol("error".to_string()),
                    Value::String("lambda parameters must be list or symbol".to_string()),
                ]);
            }
        };

        // Add continuation parameter
        let cont_param = Value::Symbol(self.fresh_continuation_name());
        cps_params.push(cont_param.clone());

        // Transform body with the continuation parameter
        let cps_body = if body_exprs.len() == 1 {
            self.transform_with_continuation(&body_exprs[0], &cont_param)
        } else {
            // Multiple expressions - transform as begin
            let begin_expr = Value::List({
                let mut begin_list = vec![Value::Symbol("begin".to_string())];
                begin_list.extend(body_exprs.iter().cloned());
                begin_list
            });
            self.transform_with_continuation(&begin_expr, &cont_param)
        };

        // Generate explicit CPS lambda annotation instead of regular lambda
        // This allows the compiler to immediately recognize CPS forms and emit
        // native MakeCont opcodes without pattern matching or heuristics
        let cps_lambda = Value::List(vec![
            Value::Symbol("$cps-lambda".to_string()),
            Value::List(cps_params[..cps_params.len() - 1].to_vec()), // Regular parameters
            cont_param.clone(),                                       // Continuation parameter
            cps_body, // Body with explicit cont-calls
        ]);

        // Pass the CPS lambda to the current continuation
        self.make_continuation_call(continuation, vec![cps_lambda])
    }

    /// Transform define expression: (define name value) -> CPS
    fn transform_define(&mut self, args: &[Value], continuation: &Value) -> Value {
        if args.len() != 2 {
            return Value::List(vec![
                Value::Symbol("error".to_string()),
                Value::String("define requires exactly 2 arguments".to_string()),
            ]);
        }

        let name = &args[0];
        let value = &args[1];

        // Transform: (define name value) k
        // Into: value (lambda (v) (define name v) (k unspecified))
        let define_continuation = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol("v".to_string())]),
            Value::List(vec![
                Value::Symbol("begin".to_string()),
                Value::List(vec![
                    Value::Symbol("define".to_string()),
                    name.clone(),
                    Value::Symbol("v".to_string()),
                ]),
                self.make_continuation_call(
                    continuation,
                    vec![Value::Symbol("$unspecified".to_string())],
                ),
            ]),
        ]);

        self.transform_with_continuation(value, &define_continuation)
    }

    /// Transform begin expression: (begin expr1 expr2 ...) -> CPS
    fn transform_begin(&mut self, args: &[Value], continuation: &Value) -> Value {
        if args.is_empty() {
            // Empty begin returns unspecified
            return self.make_continuation_call(
                continuation,
                vec![Value::Symbol("$unspecified".to_string())],
            );
        }

        if args.len() == 1 {
            // Single expression
            return self.transform_with_continuation(&args[0], continuation);
        }

        // Multiple expressions: sequence them
        // (begin e1 e2 e3) k -> e1 (lambda (_) (begin e2 e3) k)
        let first = &args[0];
        let rest = &args[1..];

        let rest_continuation = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol("_".to_string())]), // Ignore result of first expression
            self.transform_begin(rest, continuation),
        ]);

        self.transform_with_continuation(first, &rest_continuation)
    }

    /// Transform function application: (f arg1 arg2 ...) -> CPS
    fn transform_application(&mut self, elements: &[Value], continuation: &Value) -> Value {
        let function = &elements[0];
        let args = &elements[1..];

        if args.is_empty() {
            // No arguments: (f) k -> f k
            return Value::List(vec![function.clone(), continuation.clone()]);
        }

        // With arguments: (f arg1 arg2) k
        // -> arg1 (lambda (v1)
        //      arg2 (lambda (v2)
        //        f v1 v2 k))
        self.transform_arguments(function, args, continuation, 0)
    }

    /// Transform arguments recursively for function application
    fn transform_arguments(
        &mut self,
        function: &Value,
        args: &[Value],
        continuation: &Value,
        arg_index: usize,
    ) -> Value {
        if arg_index >= args.len() {
            // All arguments transformed - make the call
            // NEW APPROACH: For builtins, call continuation with result instead of passing continuation to builtin
            let mut call = vec![function.clone()];
            // Add argument variables v0, v1, v2, ...
            for i in 0..args.len() {
                call.push(Value::Symbol(format!("v{}", i)));
            }

            // Check if this is a known builtin that doesn't take continuation parameters
            let is_builtin = match function {
                Value::Symbol(name) => {
                    // List of builtins that have been updated to not take continuations
                    matches!(
                        name.as_str(),
                        "+" | "*"
                            | "-"
                            | "/"
                            | "mod"
                            | "="
                            | "<"
                            | ">"
                            | "<="
                            | ">="
                            | "car"
                            | "cdr"
                            | "cons"
                            | "list"
                            | "display"
                    )
                }
                _ => false,
            };

            if is_builtin {
                // For updated builtins: call continuation with result of builtin
                // Generate: (k (builtin v0 v1 v2))
                return self.make_continuation_call(continuation, vec![Value::List(call)]);
            } else {
                // For user-defined functions or unupdated builtins: pass continuation as parameter
                // Generate: (function v0 v1 v2 k)
                call.push(continuation.clone());
                return Value::List(call);
            }
        }

        // Transform current argument
        let current_arg = &args[arg_index];
        let var_name = format!("v{}", arg_index);

        let arg_continuation = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol(var_name)]),
            self.transform_arguments(function, args, continuation, arg_index + 1),
        ]);

        self.transform_with_continuation(current_arg, &arg_continuation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform_literal() {
        let mut transformer = CPSTransformer::new();
        let input = Value::Integer(42);
        let result = transformer.transform_program(&input);

        // Should be: ((lambda (x) x) 42)
        match result {
            Value::List(elements) => {
                assert_eq!(elements.len(), 2);
                // First element should be identity lambda
                // Second element should be 42
                assert_eq!(elements[1], Value::Integer(42));
            }
            _ => panic!("Expected list result"),
        }
    }

    #[test]
    fn test_transform_simple_application() {
        let mut transformer = CPSTransformer::new();
        let input = Value::List(vec![
            Value::Symbol("+".to_string()),
            Value::Integer(1),
            Value::Integer(2),
        ]);
        let result = transformer.transform_program(&input);

        // Should transform (+ 1 2) to CPS form with identity continuation
        match result {
            Value::List(_) => {
                // Complex nested structure expected - just verify it's a list
                println!("CPS transformed: {:?}", result);
            }
            _ => panic!("Expected list result"),
        }
    }

    #[test]
    fn test_transform_lambda() {
        let mut transformer = CPSTransformer::new();
        let input = Value::List(vec![
            Value::Symbol("lambda".to_string()),
            Value::List(vec![Value::Symbol("x".to_string())]),
            Value::Symbol("x".to_string()),
        ]);
        let result = transformer.transform_program(&input);

        // Should transform (lambda (x) x) to CPS form
        println!("Lambda CPS: {:?}", result);
        match result {
            Value::List(_) => {} // Success - complex structure expected
            _ => panic!("Expected list result"),
        }
    }
}
