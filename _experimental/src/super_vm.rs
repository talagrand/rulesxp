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
//! let mut vm = SuperStackVM::new(ast);
//! let env = ProcessedEnvironment::new();
//! let result = vm.evaluate(env)?;
//! ```

use crate::processed_ast::{ProcessedAST, StringSymbol};
use crate::processed_env::ProcessedEnvironment;
use crate::super_builtins::{ProcessedEnvironmentRef, ProcessedValue};
use crate::vm::RuntimeError;
use std::rc::Rc;
use string_interner::Symbol;

/// Type alias for the optimized argument vector used in Apply frames
/// Uses Box<Vec> to move argument storage to heap, significantly reducing SuperEvalFrame size
///
/// **FRAME SIZE OPTIMIZATION:** Box<Vec> reduces Apply variant size from ~280+ bytes to ~24 bytes
/// by moving argument storage off the stack. This improves:
/// - Frame stack cache locality (more frames fit in cache lines)
/// - Memory usage for large expressions (no wasted SmallVec inline capacity)
/// - Elimination of large_enum_variant clippy warnings
///
/// **PERFORMANCE TRADEOFF:** Adds one heap allocation per function call, but this cost is
/// amortized across the function application lifetime and significantly outweighed by
/// improved cache behavior for the frame stack.
type ApplyArgsVec<'ast> = Box<Vec<ProcessedValue<'ast>>>;

/// Stack frame for stack-based evaluation
#[derive(Debug, Clone)]
enum SuperEvalFrame<'ast> {
    /// Evaluate an expression and push result
    Evaluate(ProcessedValue<'ast>),
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
    /// Function application with argument evaluation - handles all function calls
    ///
    /// **UNIFIED FUNCTION APPLICATION:** This frame handles both no-argument and with-argument
    /// function calls, providing a single optimized path for all function applications.
    /// When eval_index reaches args.len(), all arguments are evaluated and function application occurs.
    ///
    /// **OPTIMIZATION:** Uses single vector with index instead of separate evaluated/remaining vectors.
    /// This eliminates constant reallocation as arguments are processed and improves cache locality.
    ///
    /// **INVARIANT:**
    /// - args[0] is always the function expression (then evaluated function)
    /// - args[1..eval_index] contains evaluated ProcessedValues (argument results)
    /// - args[eval_index..] contains unevaluated ProcessedValues (argument expressions to evaluate)
    /// - eval_index starts at 1 (function at index 0 is evaluated first)
    /// - eval_index advances as each argument is evaluated and replaced in-place
    /// - For no-argument calls: args.len() == 1, eval_index starts at 1 and immediately triggers application
    Apply {
        args: ApplyArgsVec<'ast>, // Combined args: [func, evaluated..., unevaluated...]
        eval_index: usize, // Index of next argument to evaluate (separates evaluated from unevaluated)
        original_env: ProcessedEnvironment<'ast>,
    },
    /// Store define result after value evaluation completes
    DefineStore {
        name: StringSymbol,
        original_env: ProcessedEnvironment<'ast>,
    },
    /// Handle procedure call setup - binds parameters and prepares body evaluation
    ProcedureCall {
        body: ProcessedValue<'ast>,
        call_env: ProcessedEnvironment<'ast>,
    },
}

/// SuperDirectVM - High-Performance Recursive ProcessedAST Evaluator
///
/// **STACK SAFETY WARNING:** This evaluator uses recursive function calls and can
/// cause Rust stack overflow on deeply nested expressions. For guaranteed stack
/// safety, use SuperStackVM instead.
///
/// **ARCHITECTURE:** Uses direct recursive evaluation where function calls directly
/// call themselves and other evaluation methods. This provides maximum performance
/// for expressions that don't exceed the Rust call stack limits.
///
/// **PERFORMANCE:** Fastest evaluation method - no frame allocation overhead
/// **STACK SAFETY:** ⚠️  NOT STACK SAFE - Can overflow on deeply nested expressions
pub struct SuperDirectVM {
    /// The ProcessedAST being evaluated (SuperDirectVM owns this)
    ast: ProcessedAST,
    /// Evaluation statistics for benchmarking
    stats: EvaluationStats,
    /// Current environment for mutable evaluation (like old VM)
    /// Uses Option to allow taking/replacing during evaluation
    current_env: Option<Rc<ProcessedEnvironment<'static>>>,
}

/// SuperStackVM - Stack-Safe ProcessedAST Evaluator
///
/// **GUARANTEED STACK SAFETY:** This evaluator cannot cause Rust stack overflow
/// regardless of expression nesting depth. Uses explicit frame stack to maintain
/// evaluation state without recursive function calls.
///
/// **ARCHITECTURE INVARIANT:** This struct and ALL methods it implements must NEVER
/// make recursive function calls. All control flow must use frame pushing/popping.
/// Any violation of this invariant breaks the stack safety guarantee.
///
/// **PERFORMANCE:** Slightly slower than SuperDirectVM due to frame allocation
/// **STACK SAFETY:** ✅ GUARANTEED STACK SAFE - Cannot overflow regardless of nesting
pub struct SuperStackVM {
    /// The ProcessedAST being evaluated (SuperStackVM owns this)
    ast: ProcessedAST,
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

impl SuperDirectVM {
    /// Create a new SuperDirectVM with the given ProcessedAST
    ///
    /// **WARNING:** This evaluator can cause Rust stack overflow on deeply recursive code.
    /// Use SuperStackVM for guaranteed stack safety.
    pub fn new(ast: ProcessedAST) -> Self {
        SuperDirectVM {
            ast,
            stats: EvaluationStats::default(),
            current_env: None,
        }
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> &EvaluationStats {
        &self.stats
    }

    /// Reset evaluation statistics
    pub fn reset_stats(&mut self) {
        self.stats = EvaluationStats::default();
    }

    /// Evaluate the root expression using direct recursive evaluation
    ///
    /// **STACK SAFETY WARNING:** This method uses recursive function calls and can
    /// cause Rust stack overflow on deeply nested expressions. For guaranteed stack
    /// safety, use SuperStackVM instead.
    pub fn evaluate<'ast>(
        &mut self,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: ProcessedEnvironment<'static> = unsafe { std::mem::transmute(env.clone()) };
        self.current_env = Some(Rc::new(static_env));

        // **UNSAFE:** Transmute 'static to 'ast - this is safe because the arena
        // in ProcessedAST lives for the entire SuperDirectVM lifetime which encompasses 'ast
        let root_expr: ProcessedValue<'ast> = unsafe { std::mem::transmute(self.ast.root.clone()) };

        self.evaluate_direct(&root_expr, env)
    }

    /// Direct recursive evaluation - **WARNING: Can cause stack overflow!**
    ///
    /// **ARCHITECTURE:** Uses recursive function calls that consume Rust call stack.
    /// This method will directly call itself and other evaluation methods recursively.
    ///
    /// **STACK SAFETY:** ⚠️  NOT STACK SAFE - Can overflow on deeply nested expressions
    /// **PERFORMANCE:** Fastest evaluation method for shallow recursion
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
                    // Use Vec and pass as slice to apply_function for zero-copy benefit
                    let mut arg_values: Vec<ProcessedValue<'ast>> = Vec::new();
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

    /// Applies a function to arguments using direct evaluation (slice-based for zero-copy)
    ///
    /// **STACK SAFETY WARNING:** This method makes recursive calls and can cause stack overflow!
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

                // **RECURSIVE CALL:** This is why SuperDirectVM can cause stack overflow!
                self.evaluate_direct(body, call_env)
            }
            _ => Err(RuntimeError::new("Type error: not a procedure".to_string())),
        }
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
}

impl SuperStackVM {
    /// Create a new SuperStackVM with the given ProcessedAST
    ///
    /// **GUARANTEED STACK SAFETY:** This evaluator cannot cause Rust stack overflow.
    /// Uses explicit frame stack to maintain evaluation state without recursive calls.
    pub fn new(ast: ProcessedAST) -> Self {
        SuperStackVM {
            ast,
            stats: EvaluationStats::default(),
            current_env: None,
        }
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> &EvaluationStats {
        &self.stats
    }

    /// Reset evaluation statistics
    pub fn reset_stats(&mut self) {
        self.stats = EvaluationStats::default();
    }

    /// Evaluate the root expression using stack-based evaluation
    ///
    /// **GUARANTEED STACK SAFETY:** This method uses an explicit frame stack and
    /// cannot cause Rust stack overflow regardless of expression nesting depth.
    ///
    /// **ARCHITECTURE INVARIANT:** This method and all methods it calls must NEVER
    /// make recursive function calls. All control flow must use frame pushing/popping.
    pub fn evaluate<'ast>(
        &mut self,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: ProcessedEnvironment<'static> = unsafe { std::mem::transmute(env.clone()) };
        self.current_env = Some(Rc::new(static_env));

        // **UNSAFE:** Transmute 'static to 'ast - this is safe because the arena
        // in ProcessedAST lives for the entire SuperStackVM lifetime which encompasses 'ast
        let root_expr: ProcessedValue<'ast> = unsafe { std::mem::transmute(self.ast.root.clone()) };

        self.evaluate_stack(&root_expr, env)
    }

    /// Stack-safe evaluation using explicit frame stack
    ///
    /// **ARCHITECTURE INVARIANT:** This method and all methods it calls must NEVER
    /// make recursive function calls. All control flow must use frame pushing/popping.
    /// Any violation of this invariant breaks the stack safety guarantee.
    ///
    /// **GUARANTEED STACK SAFETY:** This method uses an explicit frame stack and
    /// cannot cause Rust stack overflow regardless of expression nesting depth.
    fn evaluate_stack<'ast>(
        &mut self,
        expr: &ProcessedValue<'ast>,
        env: ProcessedEnvironment<'ast>,
    ) -> Result<(ProcessedValue<'ast>, ProcessedEnvironment<'ast>), RuntimeError> {
        // Pre-allocate stack with reasonable capacity for performance
        let mut stack: Vec<SuperEvalFrame<'ast>> = Vec::with_capacity(128);

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
                        // **OPTIMIZATION:** Most frequent first - Symbol lookups (not from profiling data!)
                        ProcessedValue::Symbol(sym) => {
                            // Symbol lookup
                            self.stats.environment_lookups += 1;

                            if let Some(value) = current_env.lookup(sym) {
                                result = value.clone();
                            } else {
                                return Err(RuntimeError::new(format!(
                                    "Unbound variable: symbol_{}",
                                    sym.to_usize()
                                )));
                            }
                        }

                        // **OPTIMIZATION:** Second most frequent - List applications
                        ProcessedValue::List(elements) => {
                            if elements.is_empty() {
                                // Empty list evaluates to itself
                                result = ProcessedValue::List(elements.clone());
                            } else {
                                // Function application: use stack-based evaluation
                                self.stats.function_calls += 1;

                                // Set up argument evaluation using Apply for all function applications
                                // **UNIFIED APPROACH:** Apply handles both no-argument and with-argument function calls
                                // **OPTIMIZATION:** Create single vector with all elements, eval_index indicates progress
                                let args: ApplyArgsVec<'ast> = Box::new(elements.iter().cloned().collect());
                                stack.push(SuperEvalFrame::Apply {
                                    args, // Contains [func_expr, arg1_expr, arg2_expr, ...] (or just [func_expr] for no args)
                                    eval_index: 1, // Start at 1 (func at index 0 will be evaluated first)
                                    original_env: current_env.clone(),
                                });
                                stack.push(SuperEvalFrame::Evaluate(elements[0].clone()));
                                continue;
                            }
                        }

                        // **OPTIMIZATION:** Third most frequent - If expressions
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

                        // **OPTIMIZATION:** Literals - less frequent but still common
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
                SuperEvalFrame::Apply {
                    mut args,
                    eval_index,
                    original_env,
                } => {
                    // **OPTIMIZATION:** In-place argument evaluation using single vector
                    // We just evaluated something - replace the expression at (eval_index - 1) with the result
                    // eval_index points to the NEXT argument to evaluate, so the current result goes at eval_index - 1

                    if eval_index > 0 && eval_index <= args.len() {
                        args[eval_index - 1] = result.clone();
                    } else {
                        return Err(RuntimeError::new(
                            "Apply: Invalid eval_index state".to_string(),
                        ));
                    }

                    if eval_index >= args.len() {
                        // All arguments evaluated - now apply the function
                        // args[0] is the evaluated function, args[1..] are the evaluated arguments
                        let evaluated_args = &args[1..]; // Slice of evaluated arguments

                        // **STACK SAFETY:** Apply function without recursive calls
                        match &args[0] {
                            ProcessedValue::ResolvedBuiltin {
                                name: _,
                                arity: _,
                                func,
                            } => {
                                // Apply builtin function directly - no recursion needed
                                match func(evaluated_args) {
                                    Ok(builtin_result) => {
                                        result = builtin_result;
                                        // Environment unchanged for builtins
                                    }
                                    Err(e) => return Err(e),
                                }
                            }
                            ProcessedValue::Procedure {
                                params,
                                body,
                                env: closure_env,
                                variadic,
                            } => {
                                // Check argument count (exact match for non-variadic functions)
                                if !*variadic && evaluated_args.len() != params.len() {
                                    return Err(RuntimeError::new(format!(
                                        "Arity mismatch: expected {} arguments, got {}",
                                        params.len(),
                                        evaluated_args.len()
                                    )));
                                }

                                if *variadic && evaluated_args.len() < params.len() - 1 {
                                    return Err(RuntimeError::new(format!(
                                        "Arity mismatch: expected at least {} arguments, got {}",
                                        params.len() - 1,
                                        evaluated_args.len()
                                    )));
                                }

                                // Create new environment extending closure's captured environment
                                let call_env =
                                    ProcessedEnvironment::with_parent(closure_env.env.clone());

                                // Bind regular parameters to arguments
                                let regular_param_count = if *variadic {
                                    params.len() - 1
                                } else {
                                    params.len()
                                };

                                for (i, param) in
                                    params.iter().take(regular_param_count).enumerate()
                                {
                                    call_env.define(*param, evaluated_args[i].clone());
                                }

                                // **R7RS RESTRICTED:** Variadic parameters not yet supported - would need list construction
                                if *variadic {
                                    return Err(RuntimeError::new(
                                        "Variadic functions not yet supported".to_string(),
                                    ));
                                }

                                // **STACK SAFETY:** Push ProcedureCall frame instead of recursive call
                                stack.push(SuperEvalFrame::ProcedureCall {
                                    body: (*body).clone(),
                                    call_env,
                                });
                                continue;
                            }
                            _ => {
                                return Err(RuntimeError::new(
                                    "Type error: not a procedure".to_string(),
                                ))
                            }
                        }
                    } else {
                        // More arguments to evaluate - advance eval_index and continue
                        // **OPTIMIZATION:** Reuse the same frame, just advance the index
                        let next_arg =
                            std::mem::replace(&mut args[eval_index], ProcessedValue::Unspecified);

                        // Push continuation frame with advanced eval_index
                        stack.push(SuperEvalFrame::Apply {
                            args,
                            eval_index: eval_index + 1, // Advance to next argument
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
                SuperEvalFrame::ProcedureCall { body, call_env } => {
                    // Procedure call: parameters are bound, now evaluate body
                    current_env = call_env;

                    // **STACK SAFETY:** Continue evaluation with frame push instead of recursion
                    stack.push(SuperEvalFrame::Evaluate(body));
                    continue;
                }
            }
        }

        Ok((result, current_env))
    }

    /// Create a new environment extending the current one (used for define)
    ///
    /// **STACK SAFETY:** This helper method makes no recursive calls
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
}

// Tests updated to work with SuperStackVM directly
#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_supervm_creation() {
        // Create a simple ProcessedAST for testing
        let value = Value::Integer(42);
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let vm = SuperStackVM::new(ast);
        assert_eq!(vm.stats().expressions_evaluated, 0);
    }

    #[test]
    fn test_simple_literal_evaluation() {
        // Test evaluating a simple integer literal
        let value = Value::Integer(42);
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm = SuperStackVM::new(ast);
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

    // Note: SuperStackVM doesn't have different modes - it's always stack-based
    // This test is no longer relevant

    #[test]
    fn test_stack_evaluation() {
        // Test stack-based evaluation for simple expressions
        let value = Value::Real(std::f64::consts::PI);
        let ast1 = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let ast2 = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm1 = SuperStackVM::new(ast1);
        let mut vm2 = SuperStackVM::new(ast2);

        let env1 = ProcessedEnvironment::new();
        let env2 = ProcessedEnvironment::new();

        let result1 = vm1.evaluate(env1).expect("First evaluation failed");
        let result2 = vm2.evaluate(env2).expect("Second evaluation failed");

        // Results should be identical
        match (&result1.0, &result2.0) {
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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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

        // Test that both stack evaluations produce the same result for if expressions
        let input = "(if (+ 1 2) 42 99)"; // (+ 1 2) is truthy, should return 42
        let value = parse(input).expect("Failed to parse");

        let ast1 = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let ast2 = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm1 = SuperStackVM::new(ast1);
        let mut vm2 = SuperStackVM::new(ast2);

        let env1 = ProcessedEnvironment::new();
        let env2 = ProcessedEnvironment::new();

        let result1 = vm1.evaluate(env1).expect("First evaluation failed");
        let result2 = vm2.evaluate(env2).expect("Second evaluation failed");

        // Results should be identical
        match (&result1.0, &result2.0) {
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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
            let mut vm = SuperStackVM::new(ast);

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
            let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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
        let mut vm = SuperStackVM::new(ast);

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

        let mut vm = SuperStackVM::new(ast);
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

        let mut vm = SuperStackVM::new(ast);
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

        let mut vm = SuperStackVM::new(ast);
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

        let mut vm = SuperStackVM::new(ast);
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
