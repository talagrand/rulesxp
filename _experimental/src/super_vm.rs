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
//! - **Variadic functions**: Only fully variadic `(lambda args body)` - dot notation `(lambda (a b . rest) body)` restricted
//! - **Continuations**: No call/cc or dynamic-wind support
//! - **Variable mutation**: No set! support (all bindings immutable)
//! - **Complex forms**: Must be macro-expanded before SuperVM evaluation
//!
//! **R7RS COMPLIANT:** Proper tail calls are fully supported in Stack mode
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
//! let env = Rc::new(ProcessedEnvironment::new());
//! let result = vm.evaluate(env)?;
//! ```

use crate::processed_ast::{ProcessedAST, StringSymbol};
use crate::processed_env::{BindingValue, ProcessedEnvironment};
use crate::super_builtins::ProcessedValue;
use crate::vm::{RuntimeError, StackFrame};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use string_interner::Symbol;

/// **ENHANCED STACK TRACE CAPTURE:** Includes argument values from shared buffer
fn capture_stack_trace<'ast>(
    stack: &[SuperEvalFrame<'ast>],
    shared_args_buffer: &[ProcessedValue<'ast>],
) -> Vec<StackFrame> {
    stack
        .iter()
        .enumerate()
        .map(|(depth, frame)| {
            let function_name = match frame {
                SuperEvalFrame::Evaluate {
                    expr,
                    in_tail_position,
                } => {
                    // Enhanced expression analysis with context
                    let tail_info = if *in_tail_position { " (tail)" } else { "" };
                    match expr {
                        ProcessedValue::List(list) if !list.is_empty() => match &list[0] {
                            ProcessedValue::ResolvedBuiltin { name, .. } => {
                                let args_preview = if list.len() > 1 {
                                    format!(" with {} args", list.len() - 1)
                                } else {
                                    String::new()
                                };
                                Some(format!("builtin:{:?}{}{}", name, args_preview, tail_info))
                            }
                            ProcessedValue::Symbol(sym) => {
                                let args_preview = if list.len() > 1 {
                                    format!(" with {} args", list.len() - 1)
                                } else {
                                    String::new()
                                };
                                Some(format!(
                                    "call:symbol_{}{}{}",
                                    sym.to_usize(),
                                    args_preview,
                                    tail_info
                                ))
                            }
                            ProcessedValue::Procedure { params, .. } => {
                                let arity_info = format!(" (arity {})", params.len());
                                let args_preview = if list.len() > 1 {
                                    format!(" with {} args", list.len() - 1)
                                } else {
                                    String::new()
                                };
                                Some(format!("lambda{}{}{}", arity_info, args_preview, tail_info))
                            }
                            _ => Some(format!("expression{}", tail_info)),
                        },
                        ProcessedValue::Lambda { params, .. } => {
                            Some(format!("lambda-def (arity {}){}", params.len(), tail_info))
                        }
                        ProcessedValue::If { .. } => Some(format!("if{}", tail_info)),
                        ProcessedValue::Begin { expressions } => {
                            Some(format!("begin ({} exprs){}", expressions.len(), tail_info))
                        }
                        ProcessedValue::Define { name, .. } => {
                            Some(format!("define:symbol_{}{}", name.to_usize(), tail_info))
                        }
                        ProcessedValue::Integer(n) => Some(format!("literal:{}", n)),
                        ProcessedValue::Boolean(b) => Some(format!("literal:{}", b)),
                        ProcessedValue::Symbol(sym) => {
                            Some(format!("var:symbol_{}", sym.to_usize()))
                        }
                        _ => Some(format!("expression{}", tail_info)),
                    }
                }
                SuperEvalFrame::FreezeAndEvaluate {
                    in_tail_position, ..
                } => {
                    let tail_info = if *in_tail_position { " (tail)" } else { "" };
                    Some(format!("freeze-and-evaluate{}", tail_info))
                }
                SuperEvalFrame::Apply {
                    args_ref,
                    eval_index,
                    is_tail_position,
                    ..
                } => {
                    let tail_info = if *is_tail_position { " (tail)" } else { "" };

                    // Show evaluated arguments from shared buffer
                    let mut arg_details = Vec::new();
                    for i in (args_ref.start_index + 1)..(args_ref.start_index + eval_index) {
                        if let Some(arg_val) = shared_args_buffer.get(i) {
                            let arg_preview = match arg_val {
                                ProcessedValue::Integer(n) => format!("{}", n),
                                ProcessedValue::Boolean(b) => format!("{}", b),
                                ProcessedValue::ResolvedBuiltin { name, .. } => {
                                    format!("#{:?}", name)
                                }
                                ProcessedValue::Procedure { .. } => "λ".to_string(),
                                ProcessedValue::Symbol(s) => format!("${}", s.to_usize()),
                                _ => "?".to_string(),
                            };
                            arg_details.push(arg_preview);
                        }
                    }

                    let progress = if arg_details.is_empty() {
                        format!(
                            " ({}/{} args evaluated)",
                            eval_index.saturating_sub(1), // -1 because index 0 is the function
                            args_ref.end_index - args_ref.start_index - 1
                        )
                    } else {
                        format!(
                            " ({}/{} args: [{}])",
                            eval_index.saturating_sub(1),
                            args_ref.end_index - args_ref.start_index - 1,
                            arg_details.join(", ")
                        )
                    };

                    Some(format!("apply{}{}", progress, tail_info))
                }
                SuperEvalFrame::IfContinue {
                    in_tail_position, ..
                } => {
                    let tail_info = if *in_tail_position { " (tail)" } else { "" };
                    Some(format!("if-continue{}", tail_info))
                }
                SuperEvalFrame::BeginContinue {
                    expressions,
                    current_index,
                    in_tail_position,
                } => {
                    let tail_info = if *in_tail_position { " (tail)" } else { "" };
                    let progress = format!(" ({}/{} exprs)", current_index, expressions.len());
                    Some(format!("begin-continue{}{}", progress, tail_info))
                }
                SuperEvalFrame::DefineStore { name, .. } => {
                    Some(format!("define-store:symbol_{}", name.to_usize()))
                }
                SuperEvalFrame::LetrecDefineStore { name, .. } => {
                    Some(format!("letrec-define-store:symbol_{}", name.to_usize()))
                }
                SuperEvalFrame::ProcedureCall { is_tail_call, .. } => {
                    let tail_info = if *is_tail_call { " (tail)" } else { "" };
                    Some(format!("procedure-call{}", tail_info))
                }
                SuperEvalFrame::RestoreEnv { .. } => Some("restore-env".to_string()),
                SuperEvalFrame::LetrecContinue {
                    bindings,
                    current_index,
                    in_tail_position,
                    ..
                } => {
                    let tail_info = if *in_tail_position { " (tail)" } else { "" };
                    let progress = format!(" ({}/{} bindings)", current_index, bindings.len());
                    Some(format!("letrec-continue{}{}", progress, tail_info))
                }
            };

            StackFrame {
                function_name,
                instruction_pointer: depth,
                source_location: None, // Could be enhanced with actual source locations
            }
        })
        .collect()
}

/// **ERROR AUGMENTATION:** Add stack trace to RuntimeError from SuperEvalFrame stack
fn augment_error_with_stack_trace<'ast>(
    mut error: RuntimeError,
    stack: &[SuperEvalFrame<'ast>],
    shared_args_buffer: &[ProcessedValue<'ast>],
) -> RuntimeError {
    if error.stack_trace.is_empty() {
        error.stack_trace = capture_stack_trace(stack, shared_args_buffer);
    }
    error
}

/// **DIRECT ERROR CREATION:** Create RuntimeError with stack trace in one call
fn create_runtime_error_with_stack_trace<'ast, S: Into<String>>(
    message: S,
    stack: &[SuperEvalFrame<'ast>],
    shared_args_buffer: &[ProcessedValue<'ast>],
) -> RuntimeError {
    RuntimeError {
        message: message.into(),
        stack_trace: capture_stack_trace(stack, shared_args_buffer),
        source_location: None,
    }
}

/// Arguments for Apply frames using shared buffer indices
/// **SHARED BUFFER OPTIMIZATION:** Instead of Box<Vec> allocation per function call,
/// uses indices into SuperStackVM's shared_args_buffer to eliminate heap allocations
///
/// **MEMORY EFFICIENCY:** Reduces Apply frame heap allocations from 1 per function call to 0,
/// while maintaining the same frame size benefits as the previous Box<Vec> approach
///
/// **INVARIANT:** start_index..end_index represents the arguments in shared_args_buffer
/// - args[start_index] is the function (then evaluated function)
/// - args[start_index+1..start_index+eval_offset] are evaluated arguments  
/// - args[start_index+eval_offset..end_index] are unevaluated argument expressions
#[derive(Debug, Clone)]
struct ApplyArgsRef {
    start_index: usize,
    end_index: usize,
}

/// Stack frame for stack-based evaluation
#[derive(Debug, Clone)]
enum SuperEvalFrame<'ast> {
    /// Evaluate an expression and push result
    Evaluate {
        expr: ProcessedValue<'ast>,
        in_tail_position: bool, // True if this evaluation is in tail position
    },
    /// Freeze Mutable bindings then evaluate an expression (used after internal defines/letrec complete)
    FreezeAndEvaluate {
        expr: ProcessedValue<'ast>,
        in_tail_position: bool,
    },
    /// Continue with if evaluation (then_expr, else_expr) - test result is in evaluation result
    IfContinue {
        then_expr: ProcessedValue<'ast>,
        else_expr: Option<ProcessedValue<'ast>>,
        in_tail_position: bool, // True if branches should be evaluated in tail position
    },
    /// Continue with begin evaluation using the original Cow and current index
    BeginContinue {
        expressions: std::borrow::Cow<'ast, [ProcessedValue<'ast>]>,
        current_index: usize,
        in_tail_position: bool, // True if final expression should be evaluated in tail position
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
        args_ref: ApplyArgsRef, // Reference to arguments in shared buffer
        eval_index: usize, // Index of next argument to evaluate (separates evaluated from unevaluated)
        original_env: Rc<ProcessedEnvironment<'ast>>,
        shared_args_buffer_restore_watermark: usize, // Watermark to restore when this Apply completes
        is_tail_position: bool, // True if this is a tail call (enables optimization)
    },

    /// Store define result after value evaluation completes
    DefineStore {
        name: StringSymbol,
        original_env: Rc<ProcessedEnvironment<'ast>>,
    },
    /// Store letrec define result by updating cell after value evaluation completes
    LetrecDefineStore {
        name: StringSymbol,
        letrec_env: Rc<ProcessedEnvironment<'ast>>,
    },
    /// Handle procedure call setup - binds parameters and prepares body evaluation
    ProcedureCall {
        body: ProcessedValue<'ast>,
        call_env: Rc<ProcessedEnvironment<'ast>>,
        is_tail_call: bool, // True if this is a tail call (can reuse stack frame)
    },
    /// Restore environment after procedure call completes
    RestoreEnv { env: Rc<ProcessedEnvironment<'ast>> },
    /// Continue with letrec evaluation - evaluating bindings sequentially  
    LetrecContinue {
        bindings: std::borrow::Cow<'ast, [(StringSymbol, ProcessedValue<'ast>)]>,
        body: ProcessedValue<'ast>,
        current_index: usize,
        env: Rc<ProcessedEnvironment<'ast>>,
        in_tail_position: bool,
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

    /// Get the current environment state after evaluation
    /// Returns None if no evaluation has been performed yet
    pub fn get_current_env<'ast>(&self) -> Option<Rc<ProcessedEnvironment<'ast>>> {
        self.current_env.as_ref().map(|env| {
            // **UNSAFE:** Transmute 'static back to 'ast - this is safe because
            // the environment lifetime should match the evaluation context
            let env_ast: Rc<ProcessedEnvironment<'ast>> =
                unsafe { std::mem::transmute(Rc::clone(env)) };
            env_ast
        })
    }

    /// Get a specific expression from the compiled AST by index
    /// Used for TestEnvironment functionality to evaluate individual expressions
    pub fn get_expression(&self, index: usize) -> Option<&ProcessedValue<'static>> {
        self.ast.root().get(index)
    }

    /// Resolve a string symbol to its string value
    /// Used for TestEnvironment functionality to compare string values
    pub fn resolve_symbol(&self, symbol: StringSymbol) -> Option<&str> {
        self.ast.resolve_symbol(symbol)
    }

    /// Evaluate the root expression using direct recursive evaluation
    ///
    /// **STACK SAFETY WARNING:** This method uses recursive function calls and can
    /// cause Rust stack overflow on deeply nested expressions. For guaranteed stack
    /// safety, use SuperStackVM instead.
    pub fn evaluate<'ast>(
        &mut self,
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: Rc<ProcessedEnvironment<'static>> =
            unsafe { std::mem::transmute(Rc::clone(&env)) };
        self.current_env = Some(static_env);

        // Evaluate all expressions in the root vector, returning the last result
        let mut last_result = ProcessedValue::Boolean(true); // Default result if empty

        // Clone the expressions to avoid borrowing issues
        let expressions: Vec<ProcessedValue<'static>> = self.ast.root.clone();

        for root_expr in expressions {
            // **UNSAFE:** Transmute 'static to 'ast - this is safe because the arena
            // in ProcessedAST lives for the entire SuperDirectVM lifetime which encompasses 'ast
            let expr: ProcessedValue<'ast> = unsafe { std::mem::transmute(root_expr) };
            last_result = self.evaluate_direct(&expr, Rc::clone(&env))?;
        }

        Ok(last_result)
    }

    /// Evaluate a single ProcessedValue directly
    ///
    /// **WARNING: Can cause stack overflow!**
    /// This method evaluates a single ProcessedValue expression using direct recursion.
    pub fn evaluate_single<'ast>(
        &mut self,
        expr: &ProcessedValue<'ast>,
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: Rc<ProcessedEnvironment<'static>> =
            unsafe { std::mem::transmute(Rc::clone(&env)) };
        self.current_env = Some(static_env);

        self.evaluate_direct(expr, env)
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
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        self.stats.expressions_evaluated += 1;

        match expr {
            // Literals evaluate to themselves
            ProcessedValue::Boolean(b) => Ok(ProcessedValue::Boolean(*b)),
            ProcessedValue::Integer(i) => Ok(ProcessedValue::Integer(*i)),
            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats
            ProcessedValue::OwnedString(s) => Ok(ProcessedValue::OwnedString(s.clone())),
            ProcessedValue::OwnedSymbol(s) => Ok(ProcessedValue::OwnedSymbol(s.clone())),

            // Interned strings and symbols - return as-is (point to same arena data)
            ProcessedValue::String(sym) => Ok(ProcessedValue::String(*sym)),
            ProcessedValue::Symbol(sym) => {
                // Symbol lookup in environment
                self.stats.environment_lookups += 1;

                let sym_name = self.ast.resolve_symbol(*sym).unwrap_or("<unknown>");
                // Direct symbol lookup - no string conversion needed
                if let Some(value) = env.lookup(*sym) {
                    Ok(value.clone())
                } else {
                    Err(RuntimeError::new(format!(
                        "Unbound variable: {} (symbol_{})",
                        sym_name,
                        sym.to_usize()
                    )))
                }
            }

            // Builtin functions evaluate to themselves
            ProcessedValue::ResolvedBuiltin { .. } => Ok(expr.clone()),

            // **R7RS RESTRICTED:** Complex forms need implementation
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    // Empty list evaluates to itself
                    Ok(ProcessedValue::List(elements.clone()))
                } else {
                    // Function application: (func arg1 arg2 ...)
                    self.stats.function_calls += 1;

                    let func = &elements[0];
                    let args = &elements[1..];

                    // Evaluate function - mutations happen to env directly via RefCell
                    let func_value = self.evaluate_direct(func, Rc::clone(&env))?;

                    // Arguments are evaluated using the same environment
                    // Use Vec and pass as slice to apply_function for zero-copy benefit
                    let mut arg_values: Vec<ProcessedValue<'ast>> = Vec::new();
                    for arg in args {
                        let arg_value = self.evaluate_direct(arg, Rc::clone(&env))?;
                        arg_values.push(arg_value);
                    }

                    // Apply function with the shared environment (slice-based)
                    self.apply_function_direct(func_value, &arg_values, env)
                }
            }
            ProcessedValue::Procedure { .. } => Ok(expr.clone()),
            ProcessedValue::If {
                test,
                then_branch,
                else_branch,
            } => {
                // If form: evaluate test, then choose branch based on result
                // **R7RS SEMANTICS:** Only #f is false, everything else is true
                let test_result = self.evaluate_direct(test, Rc::clone(&env))?;

                let is_true = match test_result {
                    ProcessedValue::Boolean(false) => false,
                    _ => true, // Everything except #f is true in Scheme
                };

                if is_true {
                    self.evaluate_direct(then_branch, env)
                } else if let Some(else_expr) = else_branch {
                    self.evaluate_direct(else_expr, env)
                } else {
                    // No else branch provided, return unspecified
                    Ok(ProcessedValue::Unspecified)
                }
            }
            ProcessedValue::Define { name, value } => {
                // Define form: creates new environment extending current one
                //
                // **NEW IMMUTABLE ARCHITECTURE:** Defines create new child environments
                // rather than mutating the current environment. This fixes closure capture
                // bugs where closures would see redefined values.
                //
                // **TOP-LEVEL MUTUAL RECURSION:** For lambda definitions at top-level,
                // we use Mutable cells so that later definitions can be accessed by
                // earlier ones (e.g., is-even calling is-odd defined later).
                // This is a practical extension beyond strict R7RS REPL semantics.

                match value {
                    ProcessedValue::Lambda {
                        params,
                        body,
                        variadic,
                    } => {
                        // Lambda definition: use Mutable cell for potential mutual recursion
                        // The cell allows this function to be called by later-defined functions
                        use std::cell::RefCell;

                        let cell = Rc::new(RefCell::new(ProcessedValue::Unspecified));
                        let new_env_rc = env.extend(*name, BindingValue::Mutable(Rc::clone(&cell)));

                        // Create the actual function capturing the NEW environment (with cell)
                        // This allows the function to see itself AND later definitions
                        let actual_function = ProcessedValue::Procedure {
                            params: params.clone(),
                            body,
                            env: Rc::clone(&new_env_rc),
                            variadic: *variadic,
                        };

                        // Back-patch the cell so the function can see itself
                        new_env_rc.update_cell(*name, actual_function);

                        // Update the VM's current_env for REPL access
                        let static_env: Rc<ProcessedEnvironment<'static>> =
                            unsafe { std::mem::transmute(new_env_rc) };
                        self.current_env = Some(static_env);
                    }
                    _ => {
                        // Regular define: evaluate value then create new environment
                        let eval_value = self.evaluate_direct(value, Rc::clone(&env))?;
                        let new_env = env.extend(*name, BindingValue::Immutable(eval_value));

                        // Update the VM's current_env for REPL access
                        let static_env: Rc<ProcessedEnvironment<'static>> =
                            unsafe { std::mem::transmute(new_env) };
                        self.current_env = Some(static_env);
                    }
                }

                // Define returns unspecified in R7RS
                Ok(ProcessedValue::Unspecified)
            }
            ProcessedValue::Lambda {
                params,
                body,
                variadic,
            } => {
                // Lambda form: create a closure capturing the current environment
                // **NEW IMMUTABLE ARCHITECTURE:** Just clone the Rc, no snapshot/freeze needed
                let closure = ProcessedValue::Procedure {
                    params: params.clone(),
                    body,
                    env: Rc::clone(&env),
                    variadic: *variadic,
                };
                Ok(closure)
            }
            ProcessedValue::Quote { value } => {
                // Quote form: return the quoted value without evaluation
                Ok((*value).clone())
            }
            ProcessedValue::Begin { expressions } => {
                // Begin form: evaluate expressions sequentially, return last result
                if expressions.is_empty() {
                    Ok(ProcessedValue::Unspecified)
                } else if expressions.len() == 1 {
                    self.evaluate_direct(&expressions[0], env)
                } else {
                    // Collect all leading defines
                    let mut define_tuples = Vec::new();
                    let mut first_non_define = None;
                    for (i, expr) in expressions.iter().enumerate() {
                        if let ProcessedValue::Define { name, value } = expr {
                            define_tuples.push((*name, value));
                        } else {
                            first_non_define = Some(i);
                            break;
                        }
                    }

                    if !define_tuples.is_empty() {
                        // Use letrec (parallel) semantics for internal defines
                        // **NEW IMMUTABLE ARCHITECTURE:** Use Mutable cells, no freeze/snapshot
                        use std::cell::RefCell;

                        // Create HashMap of Mutable cells for all definitions
                        let mut letrec_bindings = HashMap::new();
                        for (name, _) in &define_tuples {
                            let cell = Rc::new(RefCell::new(ProcessedValue::Unspecified));
                            letrec_bindings.insert(*name, BindingValue::Mutable(cell));
                        }

                        // Create letrec environment with all cells - returns Rc<ProcessedEnvironment>
                        let letrec_env_rc = ProcessedEnvironment::with_bindings(
                            Some(Rc::clone(&env)),
                            letrec_bindings,
                        );

                        // Evaluate all init expressions and back-patch the cells
                        for (name, value_expr) in define_tuples.iter() {
                            let init_value =
                                self.evaluate_direct(value_expr, Rc::clone(&letrec_env_rc))?;
                            letrec_env_rc.update_cell(*name, init_value);
                        }

                        // Evaluate body expressions in letrec environment
                        let body_start = first_non_define.unwrap_or(expressions.len());
                        if body_start == expressions.len() {
                            Ok(ProcessedValue::Unspecified)
                        } else if body_start == expressions.len() - 1 {
                            self.evaluate_direct(&expressions[body_start], letrec_env_rc)
                        } else {
                            let remaining_expressions = ProcessedValue::Begin {
                                expressions: std::borrow::Cow::Owned(
                                    expressions[body_start..].to_vec(),
                                ),
                            };
                            self.evaluate_direct(&remaining_expressions, letrec_env_rc)
                        }
                    } else {
                        // No internal definitions - normal begin evaluation
                        for (i, expr) in expressions.iter().enumerate() {
                            if matches!(expr, ProcessedValue::Define { .. }) {
                                return Err(RuntimeError::new(format!(
                                    "Definitions must come before expressions in body (define at position {} after non-define expression)", i
                                )));
                            }
                        }
                        for expr in &expressions[..expressions.len() - 1] {
                            self.evaluate_direct(expr, Rc::clone(&env))?;
                        }
                        self.evaluate_direct(&expressions[expressions.len() - 1], env)
                    }
                }
            }
            ProcessedValue::Letrec { bindings, body } => {
                // **NEW IMMUTABLE ARCHITECTURE:** Letrec uses Mutable cells for mutual recursion
                // No freezing or snapshot needed - cells provide the indirection
                use std::cell::RefCell;

                // Create HashMap of Mutable cells for all bindings
                let mut letrec_bindings = HashMap::new();

                for (name, _) in bindings.iter() {
                    let cell = Rc::new(RefCell::new(ProcessedValue::Unspecified));
                    letrec_bindings.insert(*name, BindingValue::Mutable(cell));
                }

                // Create letrec environment with all cells - returns Rc<ProcessedEnvironment>
                let letrec_env_rc =
                    ProcessedEnvironment::with_bindings(Some(Rc::clone(&env)), letrec_bindings);

                // Evaluate all init expressions and back-patch the cells
                for (name, init_expr) in bindings.iter() {
                    let init_value = self.evaluate_direct(init_expr, Rc::clone(&letrec_env_rc))?;
                    letrec_env_rc.update_cell(*name, init_value);
                }

                // Evaluate body in the letrec environment
                self.evaluate_direct(body, letrec_env_rc)
            }
            ProcessedValue::Unspecified => Ok(ProcessedValue::Unspecified),
        }
    }

    /// Applies a function to arguments using direct evaluation (slice-based for zero-copy)
    ///
    /// **STACK SAFETY WARNING:** This method makes recursive calls and can cause stack overflow!
    pub fn apply_function_direct<'ast>(
        &mut self,
        func: ProcessedValue<'ast>,
        args: &[ProcessedValue<'ast>],
        _env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        match func {
            ProcessedValue::ResolvedBuiltin {
                name: _,
                arity: _,
                func,
            } => {
                // Apply builtin function directly using the function pointer
                func(&self.ast.interner, args)
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
                // **PERFORMANCE:** Use Rc::clone instead of expensive environment clone
                let mut call_env_rc = Rc::clone(&closure_env);

                // Bind parameters to arguments using extend()
                if variadic {
                    // **R7RS RESTRICTED:** Only fully variadic functions supported (lambda args body)
                    // **R7RS RESTRICTED:** Dot notation (lambda (a b . rest) body) not supported
                    if params.len() != 1 {
                        return Err(RuntimeError::new(
                            "Dot notation (mixed variadic) not supported - use fully variadic form"
                                .to_string(),
                        ));
                    }
                    // Fully variadic: (lambda args body) - all args go into a list
                    let args_list = ProcessedValue::List(Cow::Owned(args.to_vec()));
                    call_env_rc = call_env_rc.extend(params[0], BindingValue::Immutable(args_list));
                } else {
                    // Fixed parameter count - extend environment for each parameter
                    for (i, param) in params.iter().enumerate() {
                        call_env_rc =
                            call_env_rc.extend(*param, BindingValue::Immutable(args[i].clone()));
                    }
                }

                // **RECURSIVE CALL:** This is why SuperDirectVM can cause stack overflow!
                self.evaluate_direct(body, call_env_rc)
            }
            _ => Err(RuntimeError::new("Type error: not a procedure".to_string())),
        }
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

    /// Get the maximum stack depth reached during evaluation
    pub fn get_max_stack_depth(&self) -> usize {
        self.stats.max_stack_depth
    }

    /// Get the current environment state after evaluation
    /// Returns None if no evaluation has been performed yet
    pub fn get_current_env<'ast>(&self) -> Option<Rc<ProcessedEnvironment<'ast>>> {
        self.current_env.as_ref().map(|env| {
            // **UNSAFE:** Transmute 'static back to 'ast - this is safe because
            // the environment lifetime should match the evaluation context
            let env_ast: Rc<ProcessedEnvironment<'ast>> =
                unsafe { std::mem::transmute(Rc::clone(env)) };
            env_ast
        })
    }

    /// Get a specific expression from the compiled AST by index
    /// Used for TestEnvironment functionality to evaluate individual expressions
    pub fn get_expression(&self, index: usize) -> Option<&ProcessedValue<'static>> {
        self.ast.root().get(index)
    }

    /// Resolve a string symbol to its string value
    /// Used for TestEnvironment functionality to compare string values
    pub fn resolve_symbol(&self, symbol: StringSymbol) -> Option<&str> {
        self.ast.resolve_symbol(symbol)
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
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: Rc<ProcessedEnvironment<'static>> =
            unsafe { std::mem::transmute(Rc::clone(&env)) };
        self.current_env = Some(static_env);

        // Evaluate all expressions in the root vector, returning the last result
        let mut last_result = ProcessedValue::Boolean(true); // Default result if empty

        // Clone the expressions to avoid borrowing issues
        let expressions: Vec<ProcessedValue<'static>> = self.ast.root.clone();

        for root_expr in expressions {
            // **UNSAFE:** Transmute 'static to 'ast - this is safe because the arena
            // in ProcessedAST lives for the entire SuperStackVM lifetime which encompasses 'ast
            let expr: ProcessedValue<'ast> = unsafe { std::mem::transmute(root_expr) };
            last_result = self.evaluate_stack(&expr, Rc::clone(&env))?;
        }

        Ok(last_result)
    }

    /// Evaluate a single ProcessedValue using stack-based evaluation
    ///
    /// **GUARANTEED STACK SAFETY:** This method uses an explicit frame stack and
    /// cannot cause Rust stack overflow regardless of expression nesting depth.
    pub fn evaluate_single<'ast>(
        &mut self,
        expr: &ProcessedValue<'ast>,
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        // Store environment for define operations (transmute to 'static for storage)
        let static_env: Rc<ProcessedEnvironment<'static>> =
            unsafe { std::mem::transmute(Rc::clone(&env)) };
        self.current_env = Some(static_env);

        self.evaluate_stack(expr, env)
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
        env: Rc<ProcessedEnvironment<'ast>>,
    ) -> Result<ProcessedValue<'ast>, RuntimeError> {
        // Pre-allocate stack with reasonable capacity for performance
        let mut stack: Vec<SuperEvalFrame<'ast>> = Vec::with_capacity(128);

        // **SHARED BUFFER OPTIMIZATION:** Watermark-based buffer management
        // Track high-water mark and use truncate for simple, efficient space management
        let mut shared_args_buffer: Vec<ProcessedValue<'ast>> = Vec::with_capacity(128);
        let mut shared_args_buffer_watermark: usize = 0; // Track highest used index for space reuse

        let mut current_env = env;
        let mut result = ProcessedValue::Unspecified;

        // Push initial expression to evaluate
        stack.push(SuperEvalFrame::Evaluate {
            expr: expr.clone(),
            in_tail_position: false, // Root expression not in tail position
        });

        while let Some(frame) = stack.pop() {
            self.stats.max_stack_depth = self.stats.max_stack_depth.max(stack.len());

            match frame {
                SuperEvalFrame::Evaluate {
                    expr,
                    in_tail_position,
                } => {
                    self.stats.expressions_evaluated += 1;

                    match expr {
                        ProcessedValue::Symbol(sym) => {
                            // Symbol lookup
                            self.stats.environment_lookups += 1;

                            if let Some(value) = current_env.lookup(sym) {
                                result = value.clone();
                            } else {
                                return Err(create_runtime_error_with_stack_trace(
                                    format!("Unbound variable: symbol_{}", sym.to_usize()),
                                    &stack,
                                    &shared_args_buffer,
                                ));
                            }
                        }

                        ProcessedValue::List(elements) => {
                            if elements.is_empty() {
                                // Empty list evaluates to itself
                                result = ProcessedValue::List(elements.clone());
                            } else {
                                // Function application: use stack-based evaluation
                                // **TAIL POSITION LOGIC:** Pass tail position to Apply frame
                                self.stats.function_calls += 1;

                                // Set up argument evaluation using Apply for all function applications
                                // **UNIFIED APPROACH:** Apply handles both no-argument and with-argument function calls
                                // **SIMPLIFIED WATERMARK OPTIMIZATION:** Use truncate for simple space management
                                let start_index = shared_args_buffer_watermark;
                                shared_args_buffer.truncate(shared_args_buffer_watermark);
                                shared_args_buffer.extend(elements.iter().cloned());
                                let end_index = shared_args_buffer.len();
                                shared_args_buffer_watermark = end_index; // Update watermark

                                let args_ref = ApplyArgsRef {
                                    start_index,
                                    end_index,
                                };
                                stack.push(SuperEvalFrame::Apply {
                                    args_ref,      // Reference to args in shared buffer
                                    eval_index: 1, // Start at 1 (func at index 0 will be evaluated first)
                                    original_env: current_env.clone(),
                                    shared_args_buffer_restore_watermark: start_index, // Reset watermark to this level when done
                                    is_tail_position: in_tail_position, // Pass through tail position
                                });
                                stack.push(SuperEvalFrame::Evaluate {
                                    expr: elements[0].clone(),
                                    in_tail_position: false,
                                });
                                continue;
                            }
                        }

                        ProcessedValue::If {
                            test,
                            then_branch,
                            else_branch,
                        } => {
                            // If form: push continuation frame and evaluate test
                            // **TAIL POSITION LOGIC:** Pass tail position to branches
                            stack.push(SuperEvalFrame::IfContinue {
                                then_expr: (*then_branch).clone(),
                                else_expr: else_branch.map(|e| (*e).clone()),
                                in_tail_position, // Pass through tail position to branches
                            });
                            stack.push(SuperEvalFrame::Evaluate {
                                expr: (*test).clone(),
                                in_tail_position: false,
                            });
                            continue;
                        }

                        ProcessedValue::Boolean(_)
                        | ProcessedValue::Integer(_)
                        | ProcessedValue::OwnedString(_)
                        | ProcessedValue::OwnedSymbol(_)
                        | ProcessedValue::String(_)
                        | ProcessedValue::ResolvedBuiltin { .. }
                        | ProcessedValue::Procedure { .. }
                        | ProcessedValue::Unspecified => {
                            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats
                            result = expr;
                        }

                        ProcessedValue::Define { name, value } => {
                            // Define form: handle recursive function definitions specially

                            // **NEW IMMUTABLE ARCHITECTURE:** For recursive defines, use Mutable cell
                            match value {
                                ProcessedValue::Lambda {
                                    params,
                                    body,
                                    variadic,
                                } => {
                                    // Recursive function definition: use Mutable cell
                                    use maplit::hashmap;
                                    use std::cell::RefCell;

                                    let cell = Rc::new(RefCell::new(ProcessedValue::Unspecified));
                                    let new_env = ProcessedEnvironment::with_bindings(
                                        Some(current_env.clone()),
                                        hashmap! { name => BindingValue::Mutable(Rc::clone(&cell)) },
                                    );

                                    // Create the function capturing the new environment
                                    let actual_function = ProcessedValue::Procedure {
                                        params: params.clone(),
                                        body,
                                        env: new_env.clone(),
                                        variadic: *variadic,
                                    };

                                    // Back-patch the cell
                                    new_env.update_cell(name, actual_function);

                                    // Update current_env
                                    current_env = new_env;

                                    // Define returns unspecified in R7RS
                                    result = ProcessedValue::Unspecified;
                                }
                                _ => {
                                    // Regular define: evaluate value using stack then extend environment
                                    // Push a DefineStore frame to handle the result, then evaluate the value
                                    stack.push(SuperEvalFrame::DefineStore {
                                        name,
                                        original_env: current_env.clone(),
                                    });
                                    stack.push(SuperEvalFrame::Evaluate {
                                        expr: (*value).clone(),
                                        in_tail_position: false,
                                    });
                                    continue;
                                }
                            }
                        }
                        ProcessedValue::Lambda {
                            params,
                            body,
                            variadic,
                        } => {
                            // Lambda form: create a closure capturing the current environment
                            // **NEW IMMUTABLE ARCHITECTURE:** Just clone the Rc, no freeze needed
                            result = ProcessedValue::Procedure {
                                params: params.clone(),
                                body,
                                env: current_env.clone(),
                                variadic: variadic.clone(),
                            };
                        }
                        ProcessedValue::Quote { value } => {
                            // Quote form: return the quoted value without evaluation
                            result = (*value).clone();
                        }
                        ProcessedValue::Begin { expressions } => {
                            // Begin form: evaluate expressions sequentially, return last result
                            // **TAIL POSITION LOGIC:** Last expression inherits tail position
                            if expressions.is_empty() {
                                result = ProcessedValue::Unspecified;
                            } else if expressions.len() == 1 {
                                stack.push(SuperEvalFrame::Evaluate {
                                    expr: expressions[0].clone(),
                                    in_tail_position,
                                });
                                continue;
                            } else {
                                // Collect all leading defines
                                let mut define_tuples = Vec::new();
                                let mut first_non_define = None;
                                for (i, expr) in expressions.iter().enumerate() {
                                    if let ProcessedValue::Define { name, value } = expr {
                                        define_tuples.push((*name, value.clone()));
                                    } else {
                                        first_non_define = Some(i);
                                        break;
                                    }
                                }

                                if !define_tuples.is_empty() {
                                    // **NEW IMMUTABLE ARCHITECTURE:** Use letrec semantics with Mutable cells
                                    // Create HashMap of Mutable cells for all definitions
                                    let mut letrec_bindings = HashMap::new();
                                    for (name, _) in &define_tuples {
                                        let cell =
                                            Rc::new(RefCell::new(ProcessedValue::Unspecified));
                                        letrec_bindings.insert(*name, BindingValue::Mutable(cell));
                                    }

                                    // Create letrec environment - returns Rc<ProcessedEnvironment>
                                    let letrec_env_rc = ProcessedEnvironment::with_bindings(
                                        Some(Rc::clone(&current_env)),
                                        letrec_bindings,
                                    );

                                    // Push body evaluation first (bottom of stack, evaluated last)
                                    let body_start = first_non_define.unwrap_or(expressions.len());
                                    if body_start == expressions.len() {
                                        // Only defines, no body - result will be Unspecified from last LetrecDefineStore
                                    } else if body_start == expressions.len() - 1 {
                                        stack.push(SuperEvalFrame::Evaluate {
                                            expr: expressions[body_start].clone(),
                                            in_tail_position,
                                        });
                                    } else {
                                        let remaining_expressions = ProcessedValue::Begin {
                                            expressions: std::borrow::Cow::Owned(
                                                expressions[body_start..].to_vec(),
                                            ),
                                        };
                                        stack.push(SuperEvalFrame::Evaluate {
                                            expr: remaining_expressions,
                                            in_tail_position,
                                        });
                                    }

                                    // Push LetrecDefineStore frames in REVERSE order (so they execute first-to-last)
                                    for (_i, (name, value_expr)) in
                                        define_tuples.iter().rev().enumerate()
                                    {
                                        stack.push(SuperEvalFrame::LetrecDefineStore {
                                            name: *name,
                                            letrec_env: Rc::clone(&letrec_env_rc),
                                        });
                                        stack.push(SuperEvalFrame::Evaluate {
                                            expr: (*value_expr).clone(),
                                            in_tail_position: false,
                                        });
                                    }

                                    current_env = letrec_env_rc;
                                    continue;
                                } else {
                                    // No internal definitions - delegate to BeginContinue
                                    stack.push(SuperEvalFrame::BeginContinue {
                                        expressions: expressions.clone(),
                                        current_index: 1,
                                        in_tail_position,
                                    });
                                    stack.push(SuperEvalFrame::Evaluate {
                                        expr: expressions[0].clone(),
                                        in_tail_position: false,
                                    });
                                    continue;
                                }
                            }
                        }
                        ProcessedValue::Letrec { bindings, body } => {
                            // **NEW IMMUTABLE ARCHITECTURE:** Letrec with Mutable cells
                            let mut letrec_bindings = HashMap::new();
                            for (name, _) in bindings.iter() {
                                let cell = Rc::new(RefCell::new(ProcessedValue::Unspecified));
                                letrec_bindings.insert(*name, BindingValue::Mutable(cell));
                            }

                            let letrec_env_rc = ProcessedEnvironment::with_bindings(
                                Some(Rc::clone(&current_env)),
                                letrec_bindings,
                            );

                            // Push evaluation continuation for letrec
                            stack.push(SuperEvalFrame::LetrecContinue {
                                bindings: bindings.clone(),
                                body: body.clone(),
                                current_index: 0,
                                env: letrec_env_rc,
                                in_tail_position,
                            });
                            continue;
                        }
                    }
                }

                SuperEvalFrame::IfContinue {
                    then_expr,
                    else_expr,
                    in_tail_position,
                } => {
                    // The test has been evaluated, result is in 'result'
                    // **R7RS SEMANTICS:** Only #f is false, everything else is true
                    let is_true = match result {
                        ProcessedValue::Boolean(false) => false,
                        _ => true, // Everything except #f is true in Scheme
                    };

                    let chosen_expr = if is_true {
                        then_expr
                    } else if let Some(else_expr) = else_expr {
                        else_expr
                    } else {
                        // No else branch, return unspecified
                        result = ProcessedValue::Unspecified;
                        continue;
                    };

                    // Evaluate chosen branch - pass through tail position
                    stack.push(SuperEvalFrame::Evaluate {
                        expr: chosen_expr,
                        in_tail_position, // Pass through tail position
                    });
                }
                SuperEvalFrame::FreezeAndEvaluate {
                    expr,
                    in_tail_position,
                } => {
                    // **NEW IMMUTABLE ARCHITECTURE:** No freezing needed, cells stay as cells
                    // Just evaluate the expression normally
                    stack.push(SuperEvalFrame::Evaluate {
                        expr,
                        in_tail_position,
                    });
                }
                SuperEvalFrame::BeginContinue {
                    expressions,
                    current_index,
                    in_tail_position,
                } => {
                    // Continue evaluating remaining expressions in begin
                    if current_index >= expressions.len() {
                        // All expressions evaluated, result is already set
                    } else {
                        // **R7RS COMPLIANCE:** Check for illegal defines mixed with expressions
                        if matches!(expressions[current_index], ProcessedValue::Define { .. }) {
                            return Err(create_runtime_error_with_stack_trace(
                                "Definitions must come before expressions in body",
                                &stack,
                                &shared_args_buffer,
                            ));
                        }

                        if current_index == expressions.len() - 1 {
                            // Last expression - evaluate in appropriate position
                            let last_expr = expressions[current_index].clone();
                            stack.push(SuperEvalFrame::Evaluate {
                                expr: last_expr,
                                in_tail_position, // Use the tail position flag from BeginContinue
                            });
                        } else {
                            // More expressions to evaluate after this one
                            let current_expr = expressions[current_index].clone();

                            // Push another BeginContinue for the remaining expressions
                            stack.push(SuperEvalFrame::BeginContinue {
                                expressions,
                                current_index: current_index + 1,
                                in_tail_position, // Preserve tail position context
                            });

                            // Evaluate the current expression
                            stack.push(SuperEvalFrame::Evaluate {
                                expr: current_expr,
                                in_tail_position: false,
                            });
                        }
                    }
                }
                SuperEvalFrame::Apply {
                    args_ref,
                    eval_index,
                    original_env,
                    shared_args_buffer_restore_watermark,
                    is_tail_position,
                } => {
                    // **SHARED BUFFER OPTIMIZATION:** In-place argument evaluation using shared buffer indices
                    // We just evaluated something - replace the expression at (eval_index - 1) with the result
                    // eval_index points to the NEXT argument to evaluate, so the current result goes at eval_index - 1

                    let args_len = args_ref.end_index - args_ref.start_index;
                    let result_index = args_ref.start_index + eval_index - 1;

                    if eval_index > 0 && eval_index <= args_len {
                        shared_args_buffer[result_index] = result.clone();
                    } else {
                        return Err(create_runtime_error_with_stack_trace(
                            "Apply: Invalid eval_index state",
                            &stack,
                            &shared_args_buffer,
                        ));
                    }

                    if eval_index >= args_len {
                        // All arguments evaluated - now apply the function
                        let func_value = &shared_args_buffer[args_ref.start_index];
                        let evaluated_args =
                            &shared_args_buffer[args_ref.start_index + 1..args_ref.end_index];

                        // **STACK SAFETY:** Apply function without recursive calls
                        match func_value {
                            ProcessedValue::ResolvedBuiltin {
                                name: _,
                                arity: _,
                                func,
                            } => {
                                // Apply builtin function directly - no recursion needed
                                match func(&self.ast.interner, evaluated_args) {
                                    Ok(builtin_result) => {
                                        result = builtin_result;
                                        // Environment unchanged for builtins
                                    }
                                    Err(e) => {
                                        return Err(augment_error_with_stack_trace(
                                            e,
                                            &stack,
                                            &shared_args_buffer,
                                        ))
                                    }
                                }

                                // **WATERMARK RESET:** Function complete - reset watermark to reclaim space
                                shared_args_buffer_watermark = shared_args_buffer_restore_watermark;
                            }
                            ProcessedValue::Procedure {
                                params,
                                body,
                                env: closure_env,
                                variadic,
                            } => {
                                // Check argument count (exact match for non-variadic functions)
                                if !*variadic && evaluated_args.len() != params.len() {
                                    return Err(augment_error_with_stack_trace(
                                        RuntimeError::new(format!(
                                            "Arity mismatch: expected {} arguments, got {}",
                                            params.len(),
                                            evaluated_args.len()
                                        )),
                                        &stack,
                                        &shared_args_buffer,
                                    ));
                                }

                                if *variadic && evaluated_args.len() < params.len() - 1 {
                                    return Err(augment_error_with_stack_trace(
                                        RuntimeError::new(format!(
                                            "Arity mismatch: expected at least {} arguments, got {}",
                                            params.len() - 1,
                                            evaluated_args.len()
                                        )),
                                        &stack,
                                        &shared_args_buffer,
                                    ));
                                }

                                // Create environment extending closure's captured environment
                                // **NEW IMMUTABLE ARCHITECTURE:** Use extend() for each parameter
                                let mut call_env_rc = Rc::clone(closure_env);

                                // Bind parameters to arguments using extend()
                                if *variadic {
                                    // **R7RS RESTRICTED:** Only fully variadic functions supported (lambda args body)
                                    // **R7RS RESTRICTED:** Dot notation (lambda (a b . rest) body) not supported
                                    if params.len() != 1 {
                                        return Err(augment_error_with_stack_trace(
                                            RuntimeError::new(
                                                "Dot notation (mixed variadic) not supported - use fully variadic form".to_string(),
                                            ),
                                            &stack,
                                            &shared_args_buffer,
                                        ));
                                    }
                                    // Fully variadic: (lambda args body) - all args go into a list
                                    let args_list =
                                        ProcessedValue::List(Cow::Owned(evaluated_args.to_vec()));
                                    call_env_rc = call_env_rc
                                        .extend(params[0], BindingValue::Immutable(args_list));
                                } else {
                                    // Fixed parameter count - extend environment for each parameter
                                    for (i, param) in params.iter().enumerate() {
                                        call_env_rc = call_env_rc.extend(
                                            *param,
                                            BindingValue::Immutable(evaluated_args[i].clone()),
                                        );
                                    }
                                }

                                // **TAIL CALL DETECTION:** Use the is_tail_position flag from Apply frame
                                let is_tail_call = is_tail_position;

                                stack.push(SuperEvalFrame::ProcedureCall {
                                    body: (*body).clone(),
                                    call_env: call_env_rc,
                                    is_tail_call,
                                });
                                // **WATERMARK RESET:** Procedure call complete - reset watermark to reclaim space
                                shared_args_buffer_watermark = shared_args_buffer_restore_watermark;
                                continue;
                            }
                            _ => {
                                return Err(augment_error_with_stack_trace(
                                    RuntimeError::new("Type error: not a procedure".to_string()),
                                    &stack,
                                    &shared_args_buffer,
                                ))
                            }
                        }
                    } else {
                        // More arguments to evaluate - advance eval_index and continue
                        // **SHARED BUFFER OPTIMIZATION:** Get next arg from buffer and continue
                        let next_arg_index = args_ref.start_index + eval_index;
                        let next_arg = shared_args_buffer[next_arg_index].clone();

                        // Push continuation frame with advanced eval_index
                        stack.push(SuperEvalFrame::Apply {
                            args_ref,
                            eval_index: eval_index + 1, // Advance to next argument
                            original_env: original_env.clone(),
                            shared_args_buffer_restore_watermark, // Preserve the same restore point
                            is_tail_position,                     // Preserve tail position flag
                        });

                        // Use original environment for argument evaluation
                        current_env = original_env;

                        // Evaluate the next argument
                        stack.push(SuperEvalFrame::Evaluate {
                            expr: next_arg,
                            in_tail_position: false,
                        });
                        continue;
                    }
                }
                SuperEvalFrame::ProcedureCall {
                    body,
                    call_env,
                    is_tail_call,
                } => {
                    // Procedure call: parameters are bound, now evaluate body
                    // **ENVIRONMENT HANDLING:** Set current_env to call_env for body evaluation.
                    // No need for RestoreEnv - the Apply frame that called this procedure
                    // already has its own environment saved and will use it when evaluating
                    // subsequent expressions or arguments.
                    current_env = call_env.clone();

                    // **PROCEDURE BODY IN TAIL POSITION:** All procedure bodies are evaluated in tail position
                    // This enables tail call optimization within procedure bodies
                    stack.push(SuperEvalFrame::Evaluate {
                        expr: body,
                        in_tail_position: true,
                    });
                    continue;
                }
                SuperEvalFrame::DefineStore { name, original_env } => {
                    // Define form: value has been evaluated, now extend environment
                    let eval_value = result;

                    // **NEW IMMUTABLE ARCHITECTURE:** Create new environment extending original
                    current_env = original_env.extend(name, BindingValue::Immutable(eval_value));

                    // Define returns unspecified in R7RS
                    result = ProcessedValue::Unspecified;
                }
                SuperEvalFrame::LetrecDefineStore { name, letrec_env } => {
                    // Internal define/letrec: value has been evaluated, now back-patch cell
                    let eval_value = result;

                    // **NEW IMMUTABLE ARCHITECTURE:** Update Mutable cell
                    letrec_env.update_cell(name, eval_value);

                    // Define returns unspecified in R7RS
                    result = ProcessedValue::Unspecified;
                }
                SuperEvalFrame::RestoreEnv { env } => {
                    // Restore the previous environment after procedure call completes
                    current_env = env;
                }
                SuperEvalFrame::LetrecContinue {
                    bindings,
                    body,
                    current_index,
                    env,
                    in_tail_position,
                } => {
                    // Continue letrec evaluation - update current binding with evaluated result
                    if current_index > 0 {
                        // We just evaluated an init expression, store its result by updating cell
                        let (name, _) = &bindings[current_index - 1];

                        // **NEW IMMUTABLE ARCHITECTURE:** Just update the cell with the result
                        env.update_cell(*name, result.clone());
                    }

                    // Evaluate next binding if any remain
                    if current_index < bindings.len() {
                        let (_, init_expr) = &bindings[current_index];

                        // Continue with next binding
                        stack.push(SuperEvalFrame::LetrecContinue {
                            bindings: bindings.clone(),
                            body: body.clone(),
                            current_index: current_index + 1,
                            env: Rc::clone(&env),
                            in_tail_position,
                        });

                        // Evaluate the init expression in the LETREC environment (not parent)
                        // This enables true parallel binding and mutual recursion
                        stack.push(SuperEvalFrame::Evaluate {
                            expr: init_expr.clone(),
                            in_tail_position: false,
                        });
                        current_env = Rc::clone(&env);
                        continue;
                    } else {
                        // All bindings evaluated - cells have been updated, ready to evaluate body
                        // **NEW IMMUTABLE ARCHITECTURE:** No freezing or fixup needed
                        current_env = env;
                        stack.push(SuperEvalFrame::Evaluate {
                            expr: body,
                            in_tail_position,
                        });
                        continue;
                    }
                }
            }
        }

        Ok(result)
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
        let env = Rc::new(ProcessedEnvironment::new());

        let result = vm.evaluate(env);
        assert!(result.is_ok());

        let result_value = result.unwrap();
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
        let value = Value::Integer(42); // **R7RS RESTRICTED:** Only i64 integers supported
        let ast1 = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let ast2 = ProcessedAST::compile(&value).expect("Failed to compile AST");

        let mut vm1 = SuperStackVM::new(ast1);
        let mut vm2 = SuperStackVM::new(ast2);

        let env1 = Rc::new(ProcessedEnvironment::new());
        let env2 = Rc::new(ProcessedEnvironment::new());

        let result1 = vm1.evaluate(env1).expect("First evaluation failed");
        let result2 = vm2.evaluate(env2).expect("Second evaluation failed");

        // Results should be identical
        match (&result1, &result2) {
            (ProcessedValue::Integer(i1), ProcessedValue::Integer(i2)) => {
                assert_eq!(i1, i2);
            }
            _ => panic!("Expected integer values from both evaluations"),
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env1 = Rc::new(ProcessedEnvironment::new());
        let env2 = Rc::new(ProcessedEnvironment::new());

        let result1 = vm1.evaluate(env1).expect("First evaluation failed");
        let result2 = vm2.evaluate(env2).expect("Second evaluation failed");

        // Results should be identical
        match (&result1, &result2) {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Define should return unspecified
        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Define should return unspecified
        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        // Lambda should create a procedure
        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

            let env = Rc::new(ProcessedEnvironment::new());
            let result = vm.evaluate(env).expect("Evaluation failed");

            match result {
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

            let env = Rc::new(ProcessedEnvironment::new());
            let result = vm.evaluate(env).expect("Evaluation failed");

            match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
            ProcessedValue::Integer(n) => assert_eq!(n, 10),
            other => panic!("Expected integer 10, got {:?}", other),
        }

        // Test cdr (rest of list)
        let input = "(cdr (list 10 20 30))";
        let value = parse(input).expect("Failed to parse");
        let ast = ProcessedAST::compile(&value).expect("Failed to compile AST");
        let mut vm = SuperStackVM::new(ast);

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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

        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).expect("Evaluation failed");

        match result {
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
        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).unwrap();

        // This should work if builtins can be returned as first-class values
        match result {
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
        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).unwrap();

        // This should return 15 if builtins work as first-class values
        match result {
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
        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).unwrap();

        // This should return 7 if builtins work as first-class values
        match result {
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
        let env = Rc::new(ProcessedEnvironment::new());
        let result = vm.evaluate(env).unwrap();

        // This should return (10) - a list with doubled value
        match result {
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
