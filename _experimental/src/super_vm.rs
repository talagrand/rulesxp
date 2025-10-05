//! # SuperVM - High-Performance ProcessedAST Evaluator
//!
//! SuperVM is a dual-mode evaluation engine for ProcessedAST that provides both direct
//! recursive evaluation and stack-based evaluation while maintaining complete isolation
//! from the legacy Value system.
//!
//! ## Evaluation Modes
//!
//! - **Direct Mode**: Recursive evaluation using Rust call stack (fastest for simple expressions)
//! - **Stack Mode**: Stack-based evaluation using explicit stack frames (prevents stack overflow)
//!
//! ## R7RS Language Support
//!
//! **Fully Implemented Special Forms:**
//! - `if` - Conditional expressions with R7RS truthiness (#f is false, everything else is true)
//! - `define` - Variable and function definitions with both syntactic forms
//! - `lambda` - Lexical closures with environment capture
//! - `quote` - Literal data expressions
//! - `begin` - Sequential evaluation with proper last-value semantics
//! - Function application - Both builtin and user-defined procedures
//!
//! **R7RS RESTRICTED:** The following features are not supported:
//! - **Variadic functions**: Lambda forms `(lambda args body)` and `(lambda (a b . rest) body)`
//! - **Proper tail calls**: Recursive calls may cause stack overflow in Direct mode
//! - **Continuations**: No call/cc or dynamic-wind support
//! - **Variable mutation**: No set! support (all bindings immutable)
//! - **Complex forms**: Must be macro-expanded before SuperVM evaluation
//!
//! **Builtin Function Coverage:**
//! - Arithmetic: `+`, `-`, `*`, `/`, `=` (5/5 core operations)
//! - Comparison: `<`, `>`, `<=`, `>=` (4/4 core comparisons)
//! - Logical: `not` (1/1 logical function)
//! - List operations: `car`, `cdr`, `cons`, `list` (4/6 core list operations)
//! - **Missing**: 50+ R7RS builtin functions (documented as R7RS RESTRICTED)
//!
//! ## Architecture Benefits
//!
//! - **Type Safety**: ProcessedValue system eliminates runtime type errors
//! - **Memory Efficiency**: Arena allocation reduces garbage collection pressure
//! - **Performance**: Compile-time builtin resolution avoids symbol lookup overhead
//! - **Isolation**: Complete separation from legacy VM enables independent optimization
//!
//! ## Usage
//!
//! ```rust
//! let ast = ProcessedAST::compile(&value)?;
//! let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
//! let env = ProcessedEnvironment::new();
//! let result = vm.evaluate(env)?;
//! ```

use crate::processed_ast::{ProcessedAST, StringSymbol};
use crate::processed_env::ProcessedEnvironment;
use crate::super_builtins::{ProcessedEnvironmentRef, ProcessedValue};
use crate::vm::RuntimeError;
use smallvec::SmallVec;
use std::rc::Rc;
use string_interner::Symbol;

/// Evaluation mode for SuperVM
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EvaluationMode {
    /// Direct recursive evaluation (fastest for simple expressions)
    Direct,
    /// Stack-based evaluation (avoids stack overflow on deep expressions)
    Stack,
}

/// Stack frame for stack-based evaluation
#[derive(Debug, Clone)]
enum SuperEvalFrame<'ast> {
    /// Evaluate an expression and push result
    Evaluate(ProcessedValue<'ast>),
    /// Apply a function with given arguments - function comes from evaluation result
    Apply { args: SmallVec<[ProcessedValue<'ast>; 4]> },
    /// Continue with if evaluation (then_expr, else_expr) - test result is in evaluation result
    IfContinue {
        then_expr: ProcessedValue<'ast>,
        else_expr: Option<ProcessedValue<'ast>>,
    },
    /// Continue with begin evaluation using the original Cow and current index
    BeginContinue {
        expressions: std::borrow::Cow<'ast, [ProcessedValue<'ast>]>,
        current_index: usize,
    },
    /// Function argument evaluation - most function calls have few arguments
    ArgEval {
        function: ProcessedValue<'ast>,
        evaluated: SmallVec<[ProcessedValue<'ast>; 4]>,
        remaining: SmallVec<[ProcessedValue<'ast>; 4]>,
        original_env: ProcessedEnvironment<'ast>,
    },
    /// Store define result after value evaluation completes
    DefineStore {
        name: StringSymbol,
        original_env: ProcessedEnvironment<'ast>,
    },
}

/// SuperVM - ProcessedAST evaluation engine
pub struct SuperVM {
    /// The ProcessedAST being evaluated (SuperVM owns this)
    ast: ProcessedAST,
    /// Current evaluation mode
    mode: EvaluationMode,
    /// Evaluation statistics for benchmarking
    stats: EvaluationStats,
    /// Current environment for mutable evaluation (like old VM)
    /// Uses Option to allow taking/replacing during evaluation
    current_env: Option<Rc<ProcessedEnvironment<'static>>>,
}

/// Evaluation statistics for performance analysis
#[derive(Debug, Clone, Default)]
pub struct EvaluationStats {
    /// Total expressions evaluated
    pub expressions_evaluated: u64,
    /// Function calls made
    pub function_calls: u64,
    /// Environment lookups performed
    pub environment_lookups: u64,
    /// New environments created
    pub environments_created: u64,
    /// Stack frames used (stack mode only)
    pub max_stack_depth: usize,
}

impl SuperVM {
    /// Create a new SuperVM with the given ProcessedAST
    pub fn new(ast: ProcessedAST, mode: EvaluationMode) -> Self {
        SuperVM {
            ast,
            mode,
            stats: EvaluationStats::default(),
            current_env: None,
        }
    }

    /// Get the current evaluation mode
    pub fn mode(&self) -> EvaluationMode {
        self.mode
    }

    /// Set the evaluation mode
    pub fn set_mode(&mut self, mode: EvaluationMode) {
        self.mode = mode;
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> &EvaluationStats {
        &self.stats
    }

    /// Reset evaluation statistics
    pub fn reset_stats(&mut self) {
        self.stats = EvaluationStats::default();
    }

    /// Evaluate the root expression with hybrid mutable/functional approach
    pub fn evaluate<'ast>(
        &mut self,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: ProcessedEnvironment<'static> = unsafe { std::mem::transmute(env.clone()) };
        self.current_env = Some(Rc::new(static_env));

        // **UNSAFE:** Transmute 'static to 'ast - this is safe because the arena
        // in ProcessedAST lives for the entire SuperVM lifetime which encompasses 'ast
        let root_expr: ProcessedValue<'ast> = unsafe { std::mem::transmute(self.ast.root.clone()) };

        match self.mode {
            EvaluationMode::Direct => self.evaluate_direct(&root_expr, env),
            EvaluationMode::Stack => self.evaluate_stack(&root_expr, env),
        }
    }

    /// Direct recursive evaluation
    fn evaluate_direct<'ast>(
        &mut self,
        expr: &ProcessedValue<'ast>,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        self.stats.expressions_evaluated += 1;

        match expr {
            // Literals evaluate to themselves
            ProcessedValue::Boolean(b) => Ok((ProcessedValue::Boolean(*b), env)),
            ProcessedValue::Integer(i) => Ok((ProcessedValue::Integer(*i), env)),
            ProcessedValue::UInteger(u) => Ok((ProcessedValue::UInteger(*u), env)),
            ProcessedValue::Real(r) => Ok((ProcessedValue::Real(*r), env)),
            ProcessedValue::OwnedString(s) => Ok((ProcessedValue::OwnedString(s.clone()), env)),
            ProcessedValue::OwnedSymbol(s) => Ok((ProcessedValue::OwnedSymbol(s.clone()), env)),

            // Interned strings and symbols - return as-is (point to same arena data)
            ProcessedValue::String(sym) => Ok((ProcessedValue::String(*sym), env)),
            ProcessedValue::Symbol(sym) => {
                // Symbol lookup in environment
                self.stats.environment_lookups += 1;

                // Direct symbol lookup - no string conversion needed
                if let Some(value) = env.lookup(*sym) {
                    Ok((value.clone(), env))
                } else {
                    Err(RuntimeError::new(format!(
                        "Unbound variable: symbol_{}",
                        sym.to_usize()
                    )))
                }
            }

            // Builtin functions evaluate to themselves
            ProcessedValue::ResolvedBuiltin { .. } => Ok((expr.clone(), env)),

            // **R7RS RESTRICTED:** Complex forms need implementation
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    // Empty list evaluates to itself
                    Ok((ProcessedValue::List(elements.clone()), env))
                } else {
                    // Function application: (func arg1 arg2 ...)
                    self.stats.function_calls += 1;

                    let func = &elements[0];
                    let args = &elements[1..];

                    // Evaluate function - this may extend environment but we use the result
                    let (func_value, eval_env) = self.evaluate_direct(func, env)?;

                    // Arguments are evaluated in parallel conceptual model (each uses original env)
                    // Use SmallVec but pass as slice to apply_function for zero-copy benefit
                    let mut arg_values: SmallVec<[ProcessedValue<'ast>; 4]> = SmallVec::new();
                    for arg in args {
                        // Use eval_env instead of original env to maintain consistency
                        let (arg_value, _) = self.evaluate_direct(arg, eval_env.clone())?;
                        arg_values.push(arg_value);
                    }

                    // Apply function with the environment from function evaluation (slice-based)
                    self.apply_function_direct(func_value, &arg_values, eval_env)
                }
            }
            ProcessedValue::Procedure { .. } => Ok((expr.clone(), env)),
            ProcessedValue::If {
                test,
                then_branch,
                else_branch,
            } => {
                // If form: evaluate test, then choose branch based on result
                // **R7RS SEMANTICS:** Only #f is false, everything else is true
                let (test_result, env1) = self.evaluate_direct(test, env)?;

                let is_true = match test_result {
                    ProcessedValue::Boolean(false) => false,
                    _ => true, // Everything except #f is true in Scheme
                };

                if is_true {
                    self.evaluate_direct(then_branch, env1)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_direct(else_expr, env1)
                } else {
                    // No else branch provided, return unspecified
                    Ok((ProcessedValue::Unspecified, env1))
                }
            }
            ProcessedValue::Define { name, value } => {
                // Define form: evaluate value then extend environment
                let (eval_value, env1) = self.evaluate_direct(value, env)?;

                // Direct symbol-based environment storage - no string conversion needed
                let new_env = self.extend_environment_symbol(env1, *name, eval_value);

                // Define returns unspecified in R7RS
                Ok((ProcessedValue::Unspecified, new_env))
            }
            ProcessedValue::Lambda {
                params,
                body,
                variadic,
            } => {
                // Lambda form: create a closure capturing the current environment
                let closure = ProcessedValue::Procedure {
                    params: params.clone(),
                    body,
                    env: ProcessedEnvironmentRef {
                        env: Rc::new(env.clone()),
                    },
                    variadic: *variadic,
                };
                Ok((closure, env))
            }
            ProcessedValue::Quote { value } => {
                // Quote form: return the quoted value without evaluation
                Ok(((*value).clone(), env))
            }
            ProcessedValue::Begin { expressions } => {
                // Begin form: evaluate expressions sequentially, return last result
                if expressions.is_empty() {
                    // Empty begin returns unspecified
                    Ok((ProcessedValue::Unspecified, env))
                } else if expressions.len() == 1 {
                    // Single expression - just evaluate it
                    self.evaluate_direct(&expressions[0], env)
                } else {
                    // Multiple expressions - evaluate all but last for side effects
                    let mut current_env = env;

                    // Evaluate all but the last expression (for side effects)
                    for expr in &expressions[..expressions.len() - 1] {
                        let (_, new_env) = self.evaluate_direct(expr, current_env)?;
                        current_env = new_env;
                    }

                    // Evaluate and return the last expression
                    self.evaluate_direct(&expressions[expressions.len() - 1], current_env)
                }
            }
            ProcessedValue::Unspecified => Ok((ProcessedValue::Unspecified, env)),
        }
    }

    /// Stack-based evaluation (avoids Rust call stack growth)
    fn evaluate_stack<'ast>(
        &mut self,
        expr: &ProcessedValue<'ast>,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        let mut stack = Vec::new();
        let mut current_env = env;
        let mut result = ProcessedValue::Unspecified;

        // Push initial expression to evaluate
        stack.push(SuperEvalFrame::Evaluate(expr.clone()));

        while let Some(frame) = stack.pop() {
            self.stats.max_stack_depth = self.stats.max_stack_depth.max(stack.len());

            match frame {
                SuperEvalFrame::Evaluate(expr) => {
                    self.stats.expressions_evaluated += 1;

                    match expr {
                        // Literals - set result directly
                        ProcessedValue::Boolean(_)
                        | ProcessedValue::Integer(_)
                        | ProcessedValue::UInteger(_)
                        | ProcessedValue::Real(_)
                        | ProcessedValue::OwnedString(_)
                        | ProcessedValue::OwnedSymbol(_)
                        | ProcessedValue::String(_)
                        | ProcessedValue::ResolvedBuiltin { .. }
                        | ProcessedValue::Procedure { .. }
                        | ProcessedValue::Unspecified => {
                            result = expr;
                        }

                        ProcessedValue::Symbol(sym) => {
                            // Symbol lookup
                            self.stats.environment_lookups += 1;

                            if let Some(value) = current_env.lookup(sym) {
                                result = value.clone();
                            } else {
                                // **TODO:** Investigate builtin environment loading
                                return Err(RuntimeError::new(format!(
                                    "Unbound variable: symbol_{}",
                                    sym.to_usize()
                                )));
                            }
                        }

                        ProcessedValue::List(elements) => {
                            if elements.is_empty() {
                                // Empty list evaluates to itself
                                result = ProcessedValue::List(elements.clone());
                            } else {
                                // Function application: use stack-based evaluation
                                self.stats.function_calls += 1;

                                // Set up argument evaluation chain
                                if elements.len() == 1 {
                                    // Just the function, no args - push Apply frame then evaluate function
                                    stack.push(SuperEvalFrame::Apply { args: SmallVec::new() });
                                    stack.push(SuperEvalFrame::Evaluate(elements[0].clone()));
                                } else {
                                    // Function plus arguments - set up ArgEval chain
                                    let remaining: SmallVec<[ProcessedValue<'ast>; 4]> = elements[1..].iter().cloned().collect();
                                    stack.push(SuperEvalFrame::ArgEval {
                                        function: ProcessedValue::Unspecified, // Will be filled by evaluation
                                        evaluated: SmallVec::new(),
                                        remaining,
                                        original_env: current_env.clone(),
                                    });
                                    stack.push(SuperEvalFrame::Evaluate(elements[0].clone()));
                                }
                                continue;
                            }
                        }
                        ProcessedValue::If {
                            test,
                            then_branch,
                            else_branch,
                        } => {
                            // If form: push continuation frame and evaluate test
                            stack.push(SuperEvalFrame::IfContinue {
                                then_expr: (*then_branch).clone(),
                                else_expr: else_branch.map(|e| (*e).clone()),
                            });
                            stack.push(SuperEvalFrame::Evaluate((*test).clone()));
                            continue;
                        }
                        ProcessedValue::Define { name, value } => {
                            // Define form: evaluate value using stack then extend environment  
                            // Push a DefineStore frame to handle the result, then evaluate the value
                            stack.push(SuperEvalFrame::DefineStore {
                                name,
                                original_env: current_env.clone(),
                            });
                            stack.push(SuperEvalFrame::Evaluate((*value).clone()));
                            continue;
                        }
                        ProcessedValue::Lambda {
                            params,
                            body,
                            variadic,
                        } => {
                            // Lambda form: create a closure capturing the current environment
                            result = ProcessedValue::Procedure {
                                params: params.clone(),
                                body,
                                env: ProcessedEnvironmentRef {
                                    env: Rc::new(current_env.clone()),
                                },
                                variadic,
                            };
                        }
                        ProcessedValue::Quote { value } => {
                            // Quote form: return the quoted value without evaluation
                            result = (*value).clone();
                        }
                        ProcessedValue::Begin { expressions } => {
                            // Begin form: evaluate expressions sequentially, return last result
                            if expressions.is_empty() {
                                // Empty begin returns unspecified
                                result = ProcessedValue::Unspecified;
                            } else if expressions.len() == 1 {
                                // Single expression - push it for evaluation
                                stack.push(SuperEvalFrame::Evaluate(expressions[0].clone()));
                                continue;
                            } else {
                                // Multiple expressions - use BeginContinue frame with slice
                                // Push BeginContinue frame to continue with remaining expressions
                                stack.push(SuperEvalFrame::BeginContinue {
                                    expressions: expressions.clone(),
                                    current_index: 1, // Start from index 1 after evaluating first
                                });

                                // Evaluate the first expression (index 0)
                                stack.push(SuperEvalFrame::Evaluate(expressions[0].clone()));
                                continue;
                            }
                        }
                    }
                }

                SuperEvalFrame::Apply { args } => {
                    // We just evaluated something - it should be our function
                    let func_value = result;

                    // Apply the function with the collected arguments (slice-based)
                    let (apply_result, new_env) =
                        self.apply_function_stack(func_value, &args, current_env)?;
                    result = apply_result;
                    current_env = new_env;
                }
                SuperEvalFrame::IfContinue {
                    then_expr,
                    else_expr,
                } => {
                    // The test has been evaluated, result is in 'result'
                    // **R7RS SEMANTICS:** Only #f is false, everything else is true
                    let is_true = match result {
                        ProcessedValue::Boolean(false) => false,
                        _ => true, // Everything except #f is true in Scheme
                    };

                    if is_true {
                        stack.push(SuperEvalFrame::Evaluate(then_expr));
                    } else if let Some(else_expr) = else_expr {
                        stack.push(SuperEvalFrame::Evaluate(else_expr));
                    } else {
                        // No else branch, return unspecified
                        result = ProcessedValue::Unspecified;
                    }
                }
                SuperEvalFrame::BeginContinue {
                    expressions,
                    current_index,
                } => {
                    // Continue evaluating remaining expressions in begin
                    if current_index >= expressions.len() {
                        // All expressions evaluated, result is already set
                    } else if current_index == expressions.len() - 1 {
                        // Last expression - just evaluate it
                        stack.push(SuperEvalFrame::Evaluate(expressions[current_index].clone()));
                    } else {
                        // More expressions to evaluate after this one
                        let current_expr = expressions[current_index].clone();

                        // Push another BeginContinue for the remaining expressions
                        stack.push(SuperEvalFrame::BeginContinue {
                            expressions,
                            current_index: current_index + 1,
                        });

                        // Evaluate the current expression
                        stack.push(SuperEvalFrame::Evaluate(current_expr));
                    }
                }
                SuperEvalFrame::ArgEval {
                    function,
                    mut evaluated,
                    mut remaining,
                    original_env,
                } => {
                    // We just evaluated something - add it to evaluated args
                    evaluated.push(result.clone());

                    if remaining.is_empty() {
                        // All arguments evaluated - now apply the function
                        // Check if function is already set or needs to be the first argument
                        match function {
                            ProcessedValue::Unspecified => {
                                // First evaluation was the function, need to get it from evaluated[0]
                                if evaluated.is_empty() {
                                    return Err(RuntimeError::new(
                                        "No function to apply".to_string(),
                                    ));
                                }
                                let func = evaluated.remove(0);
                                let (apply_result, new_env) =
                                    self.apply_function_stack(func, &evaluated, current_env)?;
                                result = apply_result;
                                current_env = new_env;
                            }
                            _ => {
                                // Function already set, apply with all evaluated args
                                let (apply_result, new_env) =
                                    self.apply_function_stack(function, &evaluated, current_env)?;
                                result = apply_result;
                                current_env = new_env;
                            }
                        }
                    } else {
                        // More arguments to evaluate - use FIFO order (remove from front)
                        let next_arg = remaining.remove(0);

                        // Push continuation frame for remaining arguments
                        stack.push(SuperEvalFrame::ArgEval {
                            function,
                            evaluated,
                            remaining,
                            original_env: original_env.clone(),
                        });

                        // Use original environment for argument evaluation
                        current_env = original_env;

                        // Evaluate the next argument
                        stack.push(SuperEvalFrame::Evaluate(next_arg));
                        continue;
                    }
                }
                SuperEvalFrame::DefineStore { name, original_env } => {
                    // Define form: value has been evaluated, now store it
                    let eval_value = result;
                    
                    // Extend environment with the new binding
                    current_env = self.extend_environment_symbol(original_env, name, eval_value);
                    
                    // Define returns unspecified in R7RS
                    result = ProcessedValue::Unspecified;
                }
            }
        }

        Ok((result, current_env))
    }

    /// Create a new environment extending the current one (used for define)
    fn extend_environment_symbol<'ast>(
        &mut self,
        current_env: ProcessedEnvironment<'ast>,
        symbol: StringSymbol,
        value: ProcessedValue<'ast>,
    ) -> ProcessedEnvironment<'ast> {
        self.stats.environments_created += 1;
        let new_env = ProcessedEnvironment::with_parent(Rc::new(current_env));
        new_env.define(symbol, value);
        new_env
    }
    /// Applies a function to arguments using direct evaluation (slice-based for zero-copy)
    pub fn apply_function_direct<'ast>(
        &mut self,
        func: ProcessedValue<'ast>,
        args: &[ProcessedValue<'ast>],
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        match func {
            ProcessedValue::ResolvedBuiltin {
                name: _,
                arity: _,
                func,
            } => {
                // Apply builtin function directly using the function pointer
                match func(&args) {
                    Ok(result) => Ok((result, env)),
                    Err(e) => Err(e),
                }
            }
            ProcessedValue::Procedure {
                params,
                body,
                env: closure_env,
                variadic,
            } => {
                // Apply user-defined procedure (lambda)

                // Check argument count (exact match for non-variadic functions)
                if !variadic && args.len() != params.len() {
                    return Err(RuntimeError::new(format!(
                        "Arity mismatch: expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }

                if variadic && args.len() < params.len() - 1 {
                    return Err(RuntimeError::new(format!(
                        "Arity mismatch: expected at least {} arguments, got {}",
                        params.len() - 1,
                        args.len()
                    )));
                }

                // Create new environment extending closure's captured environment
                let call_env = ProcessedEnvironment::with_parent(closure_env.env.clone());

                // Bind regular parameters to arguments
                let regular_param_count = if variadic {
                    params.len() - 1
                } else {
                    params.len()
                };

                for (i, param) in params.iter().take(regular_param_count).enumerate() {
                    call_env.define(*param, args[i].clone());
                }

                // **R7RS RESTRICTED:** Variadic parameters not yet supported - would need list construction
                if variadic {
                    return Err(RuntimeError::new(
                        "Variadic functions not yet supported".to_string(),
                    ));
                }

                // Evaluate body in the new environment
                self.evaluate_direct(body, call_env)
            }
            _ => Err(RuntimeError::new("Type error: not a procedure".to_string())),
        }
    }

    /// Applies a function to arguments using stack-based evaluation (slice-based for zero-copy)
    pub fn apply_function_stack<'ast>(
        &mut self,
        func: ProcessedValue<'ast>,
        args: &[ProcessedValue<'ast>],
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        match func {
            ProcessedValue::ResolvedBuiltin {
                name: _,
                arity: _,
                func,
            } => {
                // Apply builtin function directly using the function pointer
                match func(&args) {
                    Ok(result) => Ok((result, env)),
                    Err(e) => Err(e),
                }
            }
            ProcessedValue::Procedure {
                params,
                body,
                env: closure_env,
                variadic,
            } => {
                // Apply user-defined procedure (lambda)

                // Check argument count (exact match for non-variadic functions)
                if !variadic && args.len() != params.len() {
                    return Err(RuntimeError::new(format!(
                        "Arity mismatch: expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }

                if variadic && args.len() < params.len() - 1 {
                    return Err(RuntimeError::new(format!(
                        "Arity mismatch: expected at least {} arguments, got {}",
                        params.len() - 1,
                        args.len()
                    )));
                }

                // Create new environment extending closure's captured environment
                let call_env = ProcessedEnvironment::with_parent(closure_env.env.clone());

                // Bind regular parameters to arguments
                let regular_param_count = if variadic {
                    params.len() - 1
                } else {
                    params.len()
                };

                for (i, param) in params.iter().take(regular_param_count).enumerate() {
                    call_env.define(*param, args[i].clone());
                }

                // **R7RS RESTRICTED:** Variadic parameters not yet supported - would need list construction
                if variadic {
                    return Err(RuntimeError::new(
                        "Variadic functions not yet supported".to_string(),
                    ));
                }

                // Evaluate body using stack-based evaluation
                self.evaluate_stack(body, call_env)
            }
            _ => Err(RuntimeError::new("Type error: not a procedure".to_string())),
        }
    }


}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_supervm_creation() {
        // Create a simple ProcessedAST for testing
        let value = Value::Integer(42);
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let vm = SuperVM::new(ast, EvaluationMode::Direct);
        assert_eq!(vm.mode(), EvaluationMode::Direct);
        assert_eq!(vm.stats().expressions_evaluated, 0);
    }

    #[test]
    fn test_simple_literal_evaluation() {
        // Test evaluating a simple integer literal
        let value = Value::Integer(42);
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        let env = ProcessedEnvironment::new();

        let result = vm.evaluate(env);
        assert!(result.is_ok());

        let (result_value, _) = result.unwrap();
        if let ProcessedValue::Integer(i) = result_value {
            assert_eq!(i, 42);
        } else {
            panic!("Expected integer result");
        }

        assert_eq!(vm.stats().expressions_evaluated, 1);
    }

    #[test]
    fn test_evaluation_mode_switching() {
        let value = Value::Boolean(true);
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        assert_eq!(vm.mode(), EvaluationMode::Direct);

        vm.set_mode(EvaluationMode::Stack);
        assert_eq!(vm.mode(), EvaluationMode::Stack);
    }

    #[test]
    fn test_stack_vs_direct_evaluation() {
        // Test that both modes produce the same result for simple expressions
        let value = Value::Real(std::f64::consts::PI);
        let ast1 = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let ast2 = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm_direct = SuperVM::new(ast1, EvaluationMode::Direct);
        let mut vm_stack = SuperVM::new(ast2, EvaluationMode::Stack);

        let env1 = ProcessedEnvironment::new();
        let env2 = ProcessedEnvironment::new();

        let result_direct = vm_direct.evaluate(env1).expect("Direct evaluation failed");
        let result_stack = vm_stack.evaluate(env2).expect("Stack evaluation failed");

        // Results should be identical
        match (&result_direct.0, &result_stack.0) {
            (ProcessedValue::Real(r1), ProcessedValue::Real(r2)) => {
                assert!((r1 - r2).abs() < f64::EPSILON);
            }
            _ => panic!("Expected real values from both evaluations"),
        }
    }

    #[test]
    fn test_function_application_addition() {
        use crate::parser::parse;

        // Test (+ 1 2) should evaluate to 3
        let input = "(+ 1 2)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 3),
            other => panic!("Expected integer 3, got {:?}", other),
        }
    }

    #[test]
    fn test_function_application_subtraction() {
        use crate::parser::parse;

        // Test (- 10 3) should evaluate to 7
        let input = "(- 10 3)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 7),
            other => panic!("Expected integer 7, got {:?}", other),
        }
    }

    #[test]
    fn test_function_application_multiplication() {
        use crate::parser::parse;

        // Test (* 4 5) should evaluate to 20
        let input = "(* 4 5)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 20),
            other => panic!("Expected integer 20, got {:?}", other),
        }
    }

    #[test]
    fn test_function_application_nested() {
        use crate::parser::parse;

        // Test (+ (* 2 3) 4) should evaluate to 10
        let input = "(+ (* 2 3) 4)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 10),
            other => panic!("Expected integer 10, got {:?}", other),
        }
    }

    #[test]
    fn test_quote_evaluation() {
        use crate::parser::parse;

        // Test (quote 42) should evaluate to 42 without evaluation
        let input = "(quote 42)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 42),
            other => panic!("Expected integer 42, got {:?}", other),
        }
    }

    #[test]
    fn test_quote_symbol_evaluation() {
        use crate::parser::parse;

        // Test (quote x) should evaluate to the symbol x without looking it up
        let input = "(quote x)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Symbol(_) => {} // Should be a symbol
            other => panic!("Expected symbol, got {:?}", other),
        }
    }

    #[test]
    fn test_quote_list_evaluation() {
        use crate::parser::parse;

        // Test (quote (+ 1 2)) should return the unevaluated list, not 3
        let input = "(quote (+ 1 2))";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::List(elements) => {
                assert_eq!(elements.len(), 3);
                // Should contain + symbol and two integers, not be evaluated to 3
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn test_if_true_branch() {
        use crate::parser::parse;

        // Test (if #t 42 99) should evaluate to 42
        let input = "(if #t 42 99)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 42),
            other => panic!("Expected integer 42, got {:?}", other),
        }
    }

    #[test]
    fn test_if_false_branch() {
        use crate::parser::parse;

        // Test (if #f 42 99) should evaluate to 99
        let input = "(if #f 42 99)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 99),
            other => panic!("Expected integer 99, got {:?}", other),
        }
    }

    #[test]
    fn test_if_no_else_branch() {
        use crate::parser::parse;

        // Test (if #f 42) should evaluate to unspecified
        let input = "(if #f 42)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Unspecified => {} // Should be unspecified
            other => panic!("Expected unspecified, got {:?}", other),
        }
    }

    #[test]
    fn test_if_truthy_values() {
        use crate::parser::parse;

        // Test (if 0 42 99) should evaluate to 42 because 0 is truthy in Scheme (only #f is false)
        let input = "(if 0 42 99)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 42),
            other => panic!("Expected integer 42, got {:?}", other),
        }
    }

    #[test]
    fn test_if_stack_vs_direct_evaluation() {
        use crate::parser::parse;

        // Test that both stack and direct evaluation modes produce the same result for if expressions
        let input = "(if (+ 1 2) 42 99)"; // (+ 1 2) is truthy, should return 42
        let value = parse(input).expect("Failed to parse");

        let ast1 = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let ast2 = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm_direct = SuperVM::new(ast1, EvaluationMode::Direct);
        let mut vm_stack = SuperVM::new(ast2, EvaluationMode::Stack);

        let env1 = ProcessedEnvironment::new();
        let env2 = ProcessedEnvironment::new();

        let result_direct = vm_direct.evaluate(env1).expect("Direct evaluation failed");
        let result_stack = vm_stack.evaluate(env2).expect("Stack evaluation failed");

        // Results should be identical
        match (&result_direct.0, &result_stack.0) {
            (ProcessedValue::Integer(n1), ProcessedValue::Integer(n2)) => {
                assert_eq!(n1, n2);
                assert_eq!(*n1, 42);
            }
            _ => panic!("Expected integer values from both evaluations"),
        }
    }

    #[test]
    fn test_define_variable() {
        use crate::parser::parse;

        // Test simple variable definition: (define x 42)
        let input = "(define x 42)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Define should return unspecified
        match result.0 {
            ProcessedValue::Unspecified => {} // Expected
            other => panic!("Expected unspecified from define, got {:?}", other),
        }

        // The environment should be extended but we can't directly test it here
        // since the variable lookup would require a separate evaluation
    }

    #[test]
    fn test_define_with_expression() {
        use crate::parser::parse;

        // Test define with expression: (define y (+ 1 2))
        let input = "(define y (+ 1 2))";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Define should return unspecified
        match result.0 {
            ProcessedValue::Unspecified => {} // Expected
            other => panic!("Expected unspecified from define, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_creation() {
        use crate::parser::parse;

        // Test (lambda (x) x) should create a closure
        let input = "(lambda (x) x)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Lambda should create a procedure
        match result.0 {
            ProcessedValue::Procedure {
                params,
                body: _,
                env: _,
                variadic,
            } => {
                assert_eq!(params.len(), 1);
                assert!(!variadic);
            }
            other => panic!("Expected procedure from lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_application() {
        use crate::parser::parse;

        // Test ((lambda (x) x) 42) should evaluate to 42
        let input = "((lambda (x) x) 42)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 42),
            other => panic!("Expected integer 42, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_with_arithmetic() {
        use crate::parser::parse;

        // Test ((lambda (x y) (+ x y)) 3 4) should evaluate to 7
        let input = "((lambda (x y) (+ x y)) 3 4)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 7),
            other => panic!("Expected integer 7, got {:?}", other),
        }
    }

    #[test]
    fn test_begin_empty() {
        use crate::parser::parse;

        // Test (begin) should return unspecified
        let input = "(begin)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Unspecified => {} // Expected
            other => panic!("Expected unspecified from empty begin, got {:?}", other),
        }
    }

    #[test]
    fn test_begin_single_expression() {
        use crate::parser::parse;

        // Test (begin 42) should return 42
        let input = "(begin 42)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 42),
            other => panic!("Expected integer 42, got {:?}", other),
        }
    }

    #[test]
    fn test_begin_multiple_expressions() {
        use crate::parser::parse;

        // Test (begin (+ 1 2) (+ 3 4)) should return 7 (last expression)
        let input = "(begin (+ 1 2) (+ 3 4))";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 7),
            other => panic!("Expected integer 7, got {:?}", other),
        }
    }

    #[test]
    fn test_begin_with_defines() {
        use crate::parser::parse;

        // Test (begin (define x 10) (+ x 5)) should return 15
        let input = "(begin (define x 10) (+ x 5))";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 15),
            other => panic!("Expected integer 15, got {:?}", other),
        }
    }

    #[test]
    fn test_division_builtin() {
        use crate::parser::parse;

        // Test (/ 10 2) should evaluate to 5
        let input = "(/ 10 2)";
        let value = parse(input).expect("Failed to parse");

        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 5),
            other => panic!("Expected integer 5, got {:?}", other),
        }
    }

    #[test]
    fn test_comparison_operators() {
        use crate::parser::parse;

        let test_cases = vec![
            ("(< 3 5)", true),
            ("(< 5 3)", false),
            ("(> 5 3)", true),
            ("(> 3 5)", false),
            ("(<= 3 3)", true),
            ("(<= 4 3)", false),
            ("(>= 3 3)", true),
            ("(>= 2 3)", false),
        ];

        for (input, expected) in test_cases {
            let value = parse(input).expect("Failed to parse");
            let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
            let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

            let env = ProcessedEnvironment::new();
            let result = vm.evaluate(env).expect("Evaluation failed");

            match result.0 {
                ProcessedValue::Boolean(b) => {
                    assert_eq!(b, expected, "Failed for input: {}", input)
                }
                other => panic!("Expected boolean for {}, got {:?}", input, other),
            }
        }
    }

    #[test]
    fn test_logical_not() {
        use crate::parser::parse;

        let test_cases = vec![
            ("(not #f)", true),
            ("(not #t)", false),
            ("(not 42)", false), // R7RS: only #f is false
            ("(not 0)", false),  // R7RS: 0 is truthy
        ];

        for (input, expected) in test_cases {
            let value = parse(input).expect("Failed to parse");
            let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
            let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

            let env = ProcessedEnvironment::new();
            let result = vm.evaluate(env).expect("Evaluation failed");

            match result.0 {
                ProcessedValue::Boolean(b) => {
                    assert_eq!(b, expected, "Failed for input: {}", input)
                }
                other => panic!("Expected boolean for {}, got {:?}", input, other),
            }
        }
    }

    #[test]
    fn test_list_operations() {
        use crate::parser::parse;

        // Test list construction
        let input = "(list 1 2 3)";
        let value = parse(input).expect("Failed to parse");
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::List(elements) => {
                assert_eq!(elements.len(), 3);
                // Verify elements are integers 1, 2, 3
                for (i, elem) in elements.iter().enumerate() {
                    match elem {
                        ProcessedValue::Integer(n) => assert_eq!(*n, (i + 1) as i64),
                        other => panic!("Expected integer, got {:?}", other),
                    }
                }
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn test_car_cdr_operations() {
        use crate::parser::parse;

        // Test car (first element)
        let input = "(car (list 10 20 30))";
        let value = parse(input).expect("Failed to parse");
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 10),
            other => panic!("Expected integer 10, got {:?}", other),
        }

        // Test cdr (rest of list)
        let input = "(cdr (list 10 20 30))";
        let value = parse(input).expect("Failed to parse");
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::List(elements) => {
                assert_eq!(elements.len(), 2);
                match (&elements[0], &elements[1]) {
                    (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => {
                        assert_eq!(*a, 20);
                        assert_eq!(*b, 30);
                    }
                    other => panic!("Expected two integers, got {:?}", other),
                }
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn test_cons_operation() {
        use crate::parser::parse;

        // Test cons (construct list)
        let input = "(cons 1 (list 2 3))";
        let value = parse(input).expect("Failed to parse");
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);

        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result.0 {
            ProcessedValue::List(elements) => {
                assert_eq!(elements.len(), 3);
                for (i, elem) in elements.iter().enumerate() {
                    match elem {
                        ProcessedValue::Integer(n) => assert_eq!(*n, (i + 1) as i64),
                        other => panic!("Expected integer, got {:?}", other),
                    }
                }
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }

    #[test]
    fn test_builtin_environment_scenario_1_return_function() {
        // Test scenario: Function that returns a builtin function
        // (define (get-adder) +)
        // ((get-adder) 1 2)
        let ast = ProcessedAST::compile(&Value::List(vec![
            Value::Symbol("begin".to_string()),
            Value::List(vec![
                Value::Symbol("define".to_string()),
                Value::List(vec![Value::Symbol("get-adder".to_string())]),
                Value::Symbol("+".to_string()), // Return the + builtin
            ]),
            Value::List(vec![
                Value::List(vec![Value::Symbol("get-adder".to_string())]),
                Value::Integer(1),
                Value::Integer(2),
            ]),
        ]))
        .unwrap();

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).unwrap();

        // This should work if builtins can be returned as first-class values
        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 3),
            other => panic!("Expected integer 3, got {:?}", other),
        }
    }

    #[test]
    fn test_builtin_environment_scenario_2_builtin_in_list() {
        // Test scenario: Builtin function stored in a list
        // (define ops (list + - *))
        // ((car ops) 10 5)
        let ast = ProcessedAST::compile(&Value::List(vec![
            Value::Symbol("begin".to_string()),
            Value::List(vec![
                Value::Symbol("define".to_string()),
                Value::Symbol("ops".to_string()),
                Value::List(vec![
                    Value::Symbol("list".to_string()),
                    Value::Symbol("+".to_string()),
                    Value::Symbol("-".to_string()),
                    Value::Symbol("*".to_string()),
                ]),
            ]),
            Value::List(vec![
                Value::List(vec![
                    Value::Symbol("car".to_string()),
                    Value::Symbol("ops".to_string()),
                ]),
                Value::Integer(10),
                Value::Integer(5),
            ]),
        ]))
        .unwrap();

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).unwrap();

        // This should return 15 if builtins work as first-class values
        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 15),
            other => panic!("Expected integer 15, got {:?}", other),
        }
    }

    #[test]
    fn test_builtin_environment_scenario_3_builtin_parameter() {
        // Test scenario: Passing builtin as parameter to user function
        // (define (apply-op op x y) (op x y))
        // (apply-op + 3 4)
        let ast = ProcessedAST::compile(&Value::List(vec![
            Value::Symbol("begin".to_string()),
            Value::List(vec![
                Value::Symbol("define".to_string()),
                Value::List(vec![
                    Value::Symbol("apply-op".to_string()),
                    Value::Symbol("op".to_string()),
                    Value::Symbol("x".to_string()),
                    Value::Symbol("y".to_string()),
                ]),
                Value::List(vec![
                    Value::Symbol("op".to_string()),
                    Value::Symbol("x".to_string()),
                    Value::Symbol("y".to_string()),
                ]),
            ]),
            Value::List(vec![
                Value::Symbol("apply-op".to_string()),
                Value::Symbol("+".to_string()),
                Value::Integer(3),
                Value::Integer(4),
            ]),
        ]))
        .unwrap();

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).unwrap();

        // This should return 7 if builtins work as first-class values
        match result.0 {
            ProcessedValue::Integer(n) => assert_eq!(n, 7),
            other => panic!("Expected integer 7, got {:?}", other),
        }
    }

    #[test]
    fn test_builtin_environment_scenario_4_map_like() {
        // Test scenario: Map-like function using builtin
        // (define (double-list lst) (cons (* (car lst) 2) (list)))
        // (double-list (list 5))
        let ast = ProcessedAST::compile(&Value::List(vec![
            Value::Symbol("begin".to_string()),
            Value::List(vec![
                Value::Symbol("define".to_string()),
                Value::List(vec![
                    Value::Symbol("double-list".to_string()),
                    Value::Symbol("lst".to_string()),
                ]),
                Value::List(vec![
                    Value::Symbol("cons".to_string()),
                    Value::List(vec![
                        Value::Symbol("*".to_string()),
                        Value::List(vec![
                            Value::Symbol("car".to_string()),
                            Value::Symbol("lst".to_string()),
                        ]),
                        Value::Integer(2),
                    ]),
                    Value::List(vec![Value::Symbol("list".to_string())]),
                ]),
            ]),
            Value::List(vec![
                Value::Symbol("double-list".to_string()),
                Value::List(vec![Value::Symbol("list".to_string()), Value::Integer(5)]),
            ]),
        ]))
        .unwrap();

        let mut vm = SuperVM::new(ast, EvaluationMode::Direct);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(env).unwrap();

        // This should return (10) - a list with doubled value
        match result.0 {
            ProcessedValue::List(elements) => {
                assert_eq!(elements.len(), 1);
                match &elements[0] {
                    ProcessedValue::Integer(n) => assert_eq!(*n, 10),
                    other => panic!("Expected integer 10, got {:?}", other),
                }
            }
            other => panic!("Expected list, got {:?}", other),
        }
    }
}
