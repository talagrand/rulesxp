// Compiler module - converts AST to bytecode
use crate::cps::CPSTransformer;
use crate::value::{Environment, Value};
use crate::vm::{BytecodeModule, Instruction, Opcode};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub expression: Option<String>,
    pub source: Option<String>,
}

impl CompileError {
    pub fn new(message: String) -> Self {
        CompileError {
            message,
            expression: None,
            source: None,
        }
    }

    pub fn with_expression(mut self, expr: &Value) -> Self {
        self.expression = Some(format!("{}", expr));
        self
    }

    pub fn with_source(mut self, source: String) -> Self {
        self.source = Some(source);
        self
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compile error: {}", self.message)?;
        if let Some(expr) = &self.expression {
            write!(f, "\n  While compiling: {}", expr)?;
        }
        if let Some(source) = &self.source {
            write!(f, "\n  In source: {}", source)?;
        }
        Ok(())
    }
}

impl std::error::Error for CompileError {}

/// Compiler context for tracking compilation state
///
/// ## R7RS Deviations and Limitations:
///
/// **Missing Features (require compiler changes):**
/// - `set!` - all bindings are immutable after creation
/// - `letrec` - mutual recursion requires special compiler support
/// - `call/cc` and continuations - not implemented
/// - `dynamic-wind` - not implemented
///
/// **Features implemented via macro system (R7RS derived expressions):**
/// See `STANDARD_DERIVED_EXPRESSIONS` in macros.rs for the complete list.
/// These forms are expanded by the macro system before reaching the compiler.
///
/// **Architectural Limitations:**
/// - **R7RS DEVIATION:** Simplified lexical scoping - uses runtime environment capture instead of compile-time analysis
/// - **R7RS DEVIATION:** No support for proper tail call optimization
/// - **R7RS DEVIATION:** Limited error reporting with source location tracking
pub struct Compiler {
    /// Generated instructions
    instructions: Vec<Instruction>,
    /// Constant pool
    constants: Vec<Value>,
    /// String pool for variable names
    strings: Vec<String>,
    /// Source code being compiled
    source_code: String,
    /// Current source offset for debug info
    current_offset: u32,
    /// Function names being compiled (for recursion detection)
    function_stack: Vec<String>,
    /// Compile-time warnings
    warnings: Vec<String>,
    /// Current lexical environment for closure capture
    current_env: Rc<Environment>,
    /// CPS mode - emit CPS opcodes instead of regular calls
    cps_mode: bool,
}

impl Compiler {
    pub fn new(source_code: String, env: Rc<Environment>) -> Self {
        Compiler {
            instructions: Vec::new(),
            constants: Vec::new(),
            strings: Vec::new(),
            source_code,
            current_offset: 0,
            function_stack: Vec::new(),
            warnings: Vec::new(),
            current_env: env,
            cps_mode: false,
        }
    }

    /// Create a new compiler in CPS mode
    pub fn new_cps(source_code: String, env: Rc<Environment>) -> Self {
        let mut compiler = Self::new(source_code, env);
        compiler.cps_mode = true;
        compiler
    }

    /// Add a constant to the pool and return its index
    fn add_constant(&mut self, value: Value) -> usize {
        // Check if constant already exists to avoid duplicates
        for (i, existing) in self.constants.iter().enumerate() {
            if *existing == value {
                return i;
            }
        }
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Add a string to the pool and return its index
    fn add_string(&mut self, s: String) -> usize {
        // Check if string already exists to avoid duplicates
        for (i, existing) in self.strings.iter().enumerate() {
            if *existing == s {
                return i;
            }
        }
        self.strings.push(s);
        self.strings.len() - 1
    }

    /// Emit an instruction with current source offset
    fn emit(&mut self, opcode: Opcode) {
        self.instructions.push(Instruction {
            opcode,
            source_offset: self.current_offset,
        });
    }

    /// Compile a value to bytecode
    pub fn compile_value(
        &mut self,
        value: &Value,
        is_tail_position: bool,
    ) -> Result<(), CompileError> {
        match value {
            Value::Boolean(_)
            | Value::Integer(_)
            | Value::UInteger(_)
            | Value::Real(_)
            | Value::String(_)
            | Value::Unspecified => {
                // Literal values
                let const_index = self.add_constant(value.clone());
                self.emit(Opcode::LoadConst(const_index));
            }

            Value::Symbol(name) => {
                // Variable reference
                let string_index = self.add_string(name.clone());
                self.emit(Opcode::LoadVar(string_index));
            }

            Value::List(elements) if elements.is_empty() => {
                // Empty list
                self.emit(Opcode::LoadNil);
            }

            Value::List(elements) => {
                // Function call or special form
                if let Some(first) = elements.first() {
                    if let Value::Symbol(operator) = first {
                        match operator.as_str() {
                            "quote" => {
                                if elements.len() != 2 {
                                    return Err(CompileError::new(
                                        "quote requires exactly one argument".to_string(),
                                    )
                                    .with_expression(value));
                                }
                                let const_index = self.add_constant(elements[1].clone());
                                self.emit(Opcode::LoadConst(const_index));
                            }

                            "if" => {
                                if elements.len() != 3 && elements.len() != 4 {
                                    return Err(CompileError::new(
                                        "if requires 2 or 3 arguments".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                // Compile condition
                                self.compile_value(&elements[1], false)?;

                                // Jump to else branch if condition is false
                                let else_jump = self.instructions.len();
                                self.emit(Opcode::JumpIfFalse(i16::MAX)); // Placeholder - will patch this later

                                // Compile then branch
                                self.compile_value(&elements[2], is_tail_position)?;

                                if elements.len() == 4 {
                                    // Has else branch
                                    let end_jump = self.instructions.len();
                                    self.emit(Opcode::Jump(i16::MAX)); // Placeholder - will patch this later

                                    // Patch else jump to point here
                                    let else_start = self.instructions.len();
                                    if let Opcode::JumpIfFalse(ref mut offset) =
                                        &mut self.instructions[else_jump].opcode
                                    {
                                        *offset = (else_start as i32 - else_jump as i32) as i16;
                                    }

                                    // Compile else branch
                                    self.compile_value(&elements[3], is_tail_position)?;

                                    // Patch end jump
                                    let end_pos = self.instructions.len();
                                    if let Opcode::Jump(ref mut offset) =
                                        &mut self.instructions[end_jump].opcode
                                    {
                                        *offset = (end_pos as i32 - end_jump as i32) as i16;
                                    }
                                } else {
                                    // No else branch - push unspecified value
                                    let else_start = self.instructions.len();
                                    let unspecified_index = self.add_constant(Value::Unspecified);
                                    self.emit(Opcode::LoadConst(unspecified_index));

                                    // Patch else jump
                                    if let Opcode::JumpIfFalse(ref mut offset) =
                                        &mut self.instructions[else_jump].opcode
                                    {
                                        *offset = (else_start as i32 - else_jump as i32) as i16;
                                    }
                                }
                            }

                            "lambda" => {
                                if elements.len() < 3 {
                                    return Err(CompileError::new(
                                        "lambda requires at least 2 arguments".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                // Parse parameter list
                                let params = match &elements[1] {
                                    Value::List(param_list) => {
                                        let mut params = Vec::new();
                                        let variadic = false;

                                        for param in param_list {
                                            match param {
                                                Value::Symbol(name) => params.push(name.clone()),
                                                _ => {
                                                    return Err(CompileError::new(
                                                        "Parameter must be a symbol".to_string(),
                                                    )
                                                    .with_expression(value))
                                                }
                                            }
                                        }
                                        (params, variadic)
                                    }
                                    Value::Symbol(name) => {
                                        // Single parameter - variadic
                                        (vec![name.clone()], true)
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            "Parameter list must be a list or symbol".to_string(),
                                        )
                                        .with_expression(value))
                                    }
                                };

                                // Create a new compiler for the lambda body
                                let mut lambda_compiler = Compiler::new(
                                    self.source_code.clone(),
                                    self.current_env.clone(),
                                );
                                lambda_compiler.current_offset = self.current_offset;

                                // Compile lambda body (multiple expressions)
                                let body_count = elements.len() - 2;
                                for (i, expr) in elements[2..].iter().enumerate() {
                                    let is_last = i == body_count - 1;
                                    lambda_compiler.compile_value(expr, is_last)?;
                                    if !is_last {
                                        lambda_compiler.emit(Opcode::Pop); // Discard non-final results
                                    }
                                }
                                lambda_compiler.emit(Opcode::Return);

                                // Create closure value with proper environment capture
                                let closure = Value::Procedure {
                                    params: params.0,
                                    body: if elements.len() == 3 {
                                        // Single expression body
                                        Rc::new(elements[2].clone())
                                    } else {
                                        // Multiple expressions - wrap in a begin block
                                        Rc::new(Value::List(elements[2..].to_vec()))
                                    },
                                    env: self.current_env.clone(), // Capture lexical environment
                                    variadic: params.1,
                                };

                                // For now, just load the closure as a constant
                                // TODO: Implement proper closure creation with captured variables
                                let closure_index = self.add_constant(closure);
                                self.emit(Opcode::LoadConst(closure_index));
                            }

                            "define" => {
                                if elements.len() != 3 {
                                    return Err(CompileError::new(
                                        "define requires exactly 2 arguments".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                match &elements[1] {
                                    Value::Symbol(name) => {
                                        // Simple variable definition: (define x value)
                                        self.compile_value(&elements[2], false)?;
                                        let name_index = self.add_string(name.clone());
                                        self.emit(Opcode::DefineVar(name_index));
                                    }
                                    Value::List(func_def) => {
                                        // Function definition: (define (name params...) body...)
                                        if func_def.is_empty() {
                                            return Err(CompileError::new("Function definition cannot have empty parameter list".to_string()).with_expression(value));
                                        }

                                        let func_name = match &func_def[0] {
                                            Value::Symbol(name) => name.clone(),
                                            _ => {
                                                return Err(CompileError::new(
                                                    "Function name must be a symbol".to_string(),
                                                )
                                                .with_expression(value))
                                            }
                                        };

                                        // Create lambda expression: (lambda (params...) body)
                                        let lambda_params = Value::List(func_def[1..].to_vec());
                                        let lambda_expr = Value::List(vec![
                                            Value::Symbol("lambda".to_string()),
                                            lambda_params,
                                            elements[2].clone(),
                                        ]);

                                        // Track function name for recursion detection
                                        self.function_stack.push(func_name.clone());

                                        // Compile the lambda
                                        self.compile_value(&lambda_expr, false)?;

                                        // Pop function name
                                        self.function_stack.pop();

                                        // Define the function
                                        let name_index = self.add_string(func_name);
                                        self.emit(Opcode::DefineVar(name_index));
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            "define target must be symbol or function definition"
                                                .to_string(),
                                        )
                                        .with_expression(value))
                                    }
                                }

                                // Push unspecified value (define returns unspecified)
                                let unspecified_index = self.add_constant(Value::Unspecified);
                                self.emit(Opcode::LoadConst(unspecified_index));
                            }

                            "begin" => {
                                if elements.len() < 2 {
                                    return Err(CompileError::new(
                                        "begin requires at least one expression".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                // Compile all expressions, discarding all but the last result
                                let body_count = elements.len() - 1;
                                for (i, expr) in elements[1..].iter().enumerate() {
                                    let is_last = i == body_count - 1;
                                    self.compile_value(expr, is_last && is_tail_position)?;
                                    if !is_last {
                                        self.emit(Opcode::Pop); // Discard non-final results
                                    }
                                }
                            }

                            // **R7RS DEVIATION:** Block truly unsupported R7RS special forms
                            // Forms in macros::STANDARD_DERIVED_EXPRESSIONS are handled by macro system
                            "letrec"
                            | "set!"
                            | "call/cc"
                            | "call-with-current-continuation"
                            | "dynamic-wind" => {
                                let reason = match operator.as_str() {
                                    "letrec" => "requires compiler support for mutual recursion",
                                    "set!" => {
                                        "variable mutation not supported - all bindings immutable"
                                    }
                                    _ => "continuation support not implemented",
                                };
                                return Err(CompileError::new(format!(
                                    "R7RS DEVIATION: {} - {}",
                                    operator, reason
                                ))
                                .with_expression(value));
                            }

                            // === CPS INTERNAL FORMS ===
                            // These are generated by CPS transformation, not written by users
                            "$$-cps-lambda" => {
                                /*
                                 * Handle CPS lambda form: ($$-cps-lambda (params) cont-param body)
                                 *
                                 * Unlike regular lambdas, CPS lambdas:
                                 * 1. Have explicit continuation parameter (not just "last param")
                                 * 2. Generate MakeCont opcode for continuation parameter
                                 * 3. Enable direct ContJump optimization in body
                                 *
                                 * This eliminates the need for fragile signature detection or
                                 * heuristics about which parameters are continuations.
                                 */
                                if elements.len() != 4 {
                                    return Err(CompileError::new(
                                        "$$-cps-lambda requires exactly 3 arguments: (params) cont-param body".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                // Extract components: regular params, continuation param, body
                                let regular_params = &elements[1];
                                let cont_param = &elements[2];
                                let body = &elements[3];

                                // Parse regular parameters
                                let params = match regular_params {
                                    Value::List(param_list) => {
                                        let mut params = Vec::new();
                                        for param in param_list {
                                            match param {
                                                Value::Symbol(name) => params.push(name.clone()),
                                                _ => {
                                                    return Err(CompileError::new(
                                                        "CPS lambda parameters must be symbols"
                                                            .to_string(),
                                                    )
                                                    .with_expression(value))
                                                }
                                            }
                                        }
                                        (params, false) // No variadic support in CPS for now
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            "CPS lambda parameters must be a list".to_string(),
                                        )
                                        .with_expression(value))
                                    }
                                };

                                // Emit MakeCont for continuation parameter
                                // This creates a continuation object that can be called with ContJump
                                // 
                                // TODO: CRITICAL - Calculate proper continuation target and captured variables:
                                // - target_ip: Should point to the start of the continuation's body code
                                // - captured_vars: Should count free variables in the continuation closure
                                // - This requires proper continuation analysis and closure capture detection
                                // 
                                // CURRENT STATE: Using MAX placeholders that will cause obvious runtime errors
                                // This prevents subtle bugs but requires implementing proper continuation compilation
                                self.emit(Opcode::MakeCont(usize::MAX, u8::MAX)); // Placeholders - will cause obvious errors if not fixed

                                // Compile the CPS lambda body
                                // The body should contain $$-cont-call forms that will emit ContJump
                                let mut lambda_compiler = Compiler::new(
                                    self.source_code.clone(),
                                    self.current_env.clone(),
                                );
                                lambda_compiler.current_offset = self.current_offset;
                                lambda_compiler.cps_mode = true; // Ensure CPS compilation for body

                                lambda_compiler.compile_value(body, true)?; // Body is in tail position
                                lambda_compiler.emit(Opcode::Return);

                                // Create the CPS procedure
                                let mut all_params = params.0;
                                if let Value::Symbol(cont_name) = cont_param {
                                    all_params.push(cont_name.clone());
                                }

                                let closure = Value::Procedure {
                                    params: all_params,
                                    body: Rc::new(body.clone()),
                                    env: self.current_env.clone(),
                                    variadic: params.1,
                                };

                                let closure_index = self.add_constant(closure);
                                self.emit(Opcode::LoadConst(closure_index));
                            }

                            "$$-cont-call" => {
                                /*
                                 * Handle continuation call: ($$-cont-call k arg1 arg2 ...)
                                 *
                                 * This is THE key optimization that makes our CPS implementation native.
                                 * Instead of treating continuation calls as regular function calls,
                                 * we emit optimized continuation calls.
                                 *
                                 * For identity continuations (lambda (x) x), we can just return the value.
                                 * For complex continuations, we'd use ContJump or CallCont.
                                 */
                                if elements.len() < 2 {
                                    return Err(CompileError::new(
                                        "$$-cont-call requires at least continuation argument"
                                            .to_string(),
                                    )
                                    .with_expression(value));
                                }

                                // Check if this is an identity continuation: (lambda (x) x)
                                let continuation = &elements[1];
                                let is_identity_continuation = match continuation {
                                    Value::List(cont_elements) if cont_elements.len() == 3 => {
                                        if let (Value::Symbol(op), Value::List(params), Value::Symbol(body)) = 
                                            (&cont_elements[0], &cont_elements[1], &cont_elements[2]) {
                                            op == "lambda" && params.len() == 1 && 
                                            if let Value::Symbol(param) = &params[0] {
                                                body == param
                                            } else {
                                                false
                                            }
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                };

                                if is_identity_continuation {
                                    // Identity continuation - just compile the argument and return it
                                    if elements.len() > 2 {
                                        self.compile_value(&elements[2], is_tail_position)?;
                                    }
                                    // Value is already on stack, no jump needed
                                } else {
                                    // Complex continuation - use CallCont for now
                                    // 
                                    // TODO: OPTIMIZATION - Implement direct ContJump for known continuations:
                                    // - Analyze continuation structure to determine if it's a simple jump target
                                    // - Calculate proper jump offset for direct continuation calls
                                    // - Emit ContJump(offset) instead of CallCont for better performance
                                    // - This would eliminate function call overhead for continuation calls
                                    //
                                    // CURRENT STATE: Using CallCont as fallback (works but less optimal)
                                    for arg in &elements[2..] {
                                        self.compile_value(arg, false)?;
                                    }
                                    self.compile_value(continuation, false)?;
                                    self.emit(Opcode::CallCont((elements.len() - 2) as u8));
                                }
                            }

                            _ => {
                                // Function call - check for recursion
                                self.check_recursion(operator, is_tail_position)?;

                                // Compile arguments
                                for arg in &elements[1..] {
                                    self.compile_value(arg, false)?;
                                }

                                // Compile function
                                self.compile_value(&elements[0], false)?;

                                // Emit call instruction
                                let arg_count = elements.len() - 1;
                                if arg_count > u8::MAX as usize {
                                    return Err(CompileError::new(
                                        "Too many arguments".to_string(),
                                    )
                                    .with_expression(value));
                                }

                                if self.cps_mode {
                                    // In CPS mode, last argument is continuation, so we need CallCont
                                    // CallCont expects: args... continuation
                                    if arg_count == 0 {
                                        return Err(CompileError::new(
                                            "CPS call requires at least a continuation argument"
                                                .to_string(),
                                        )
                                        .with_expression(value));
                                    }
                                    self.emit(Opcode::CallCont((arg_count - 1) as u8));
                                } else if is_tail_position {
                                    self.emit(Opcode::TailCall(arg_count as u8));
                                } else {
                                    self.emit(Opcode::Call(arg_count as u8));
                                }
                            }
                        }
                    } else {
                        // Non-symbol function expression (like lambda calls)
                        // Compile arguments
                        for arg in &elements[1..] {
                            self.compile_value(arg, false)?;
                        }

                        // Compile function expression
                        self.compile_value(&elements[0], false)?;

                        // Emit call instruction
                        let arg_count = elements.len() - 1;
                        if arg_count > u8::MAX as usize {
                            return Err(CompileError::new("Too many arguments".to_string())
                                .with_expression(value));
                        }

                        if self.cps_mode {
                            // In CPS mode, last argument is continuation, so we need CallCont
                            if arg_count == 0 {
                                return Err(CompileError::new(
                                    "CPS call requires at least a continuation argument"
                                        .to_string(),
                                )
                                .with_expression(value));
                            }
                            self.emit(Opcode::CallCont((arg_count - 1) as u8));
                        } else if is_tail_position {
                            self.emit(Opcode::TailCall(arg_count as u8));
                        } else {
                            self.emit(Opcode::Call(arg_count as u8));
                        }
                    }
                } else {
                    return Err(CompileError::new("Invalid function call".to_string())
                        .with_expression(value));
                }
            }

            _ => {
                return Err(CompileError::new(format!(
                    "Cannot compile value: {:?}",
                    value
                )));
            }
        }

        Ok(())
    }

    /// Check for potential recursion and emit warnings
    fn check_recursion(
        &mut self,
        function_name: &str,
        is_tail_position: bool,
    ) -> Result<(), CompileError> {
        // Check if we're currently compiling this function (recursion)
        if self.function_stack.contains(&function_name.to_string()) && !is_tail_position {
            let warning = format!(
                "Warning: Non-tail recursive call to '{}' at offset {} - consider using tail recursion",
                function_name,
                self.current_offset
            );
            self.warnings.push(warning);
        }
        Ok(())
    }

    /// Finish compilation and return bytecode module
    pub fn finish(self) -> BytecodeModule {
        // Print compile-time warnings
        for warning in &self.warnings {
            eprintln!("{}", warning);
        }

        BytecodeModule {
            code: self.instructions,
            constants: self.constants,
            strings: self.strings,
            source_code: self.source_code,
            entry_point: 0,
        }
    }
}

/// Compile a value to bytecode
pub fn compile(
    value: &Value,
    source_code: String,
    env: Rc<Environment>,
) -> Result<BytecodeModule, CompileError> {
    let mut compiler = Compiler::new(source_code, env);
    compiler.compile_value(value, true)?; // Top-level is in tail position
    compiler.emit(Opcode::Return);
    Ok(compiler.finish())
}

/// Compile with automatic CPS transformation
/// This is the recommended compilation path for Phase 2+ of the CPS implementation
pub fn compile_with_cps(
    value: &Value,
    source_code: String,
    env: Rc<Environment>,
) -> Result<BytecodeModule, CompileError> {
    let mut cps_transformer = CPSTransformer::new();
    let cps_value = cps_transformer.transform_program(value);

    // Use CPS-aware compiler that emits CPS opcodes
    let mut compiler = Compiler::new_cps(source_code.clone(), env);
    compiler.compile_value(&cps_value, true)?; // Top-level is in tail position

    Ok(compiler.finish())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_compile_literal() {
        let source = "42";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        assert_eq!(module.code.len(), 2); // LoadConst + Return
        assert_eq!(module.constants.len(), 1);
        assert_eq!(module.constants[0], Value::Integer(42));
    }

    #[test]
    fn test_compile_symbol() {
        let source = "foo";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        assert_eq!(module.code.len(), 2); // LoadVar + Return
        assert_eq!(module.strings.len(), 1);
        assert_eq!(module.strings[0], "foo");
    }

    #[test]
    fn test_compile_empty_list() {
        let source = "()";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        assert_eq!(module.code.len(), 2); // LoadNil + Return
    }

    #[test]
    fn test_compile_quote() {
        let source = "'foo";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        assert_eq!(module.code.len(), 2); // LoadConst + Return
        assert_eq!(module.constants.len(), 1);
        assert_eq!(module.constants[0], Value::Symbol("foo".to_string()));
    }

    #[test]
    fn test_compile_if_statement() {
        let source = "(if #t 42 24)";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        // Should have: LoadConst(#t), JumpIfFalse, LoadConst(42), Jump, LoadConst(24), Return
        assert!(module.code.len() >= 5);
        assert_eq!(module.constants[0], Value::Boolean(true));
        assert_eq!(module.constants[1], Value::Integer(42));
        assert_eq!(module.constants[2], Value::Integer(24));
    }

    #[test]
    fn test_compile_lambda() {
        let source = "(lambda (x) (+ x 1))";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        // Should have: LoadConst + Return (the lambda creates a procedure constant)
        assert_eq!(module.code.len(), 2);
        assert_eq!(module.constants.len(), 1);
        match &module.constants[0] {
            Value::Procedure { .. } => {} // Success
            _ => panic!("Expected procedure constant"),
        }
    }

    #[test]
    fn test_compile_define_variable() {
        let source = "(define x 42)";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        // Should have: LoadConst(42), DefineVar, LoadConst(unspecified), Return
        assert_eq!(module.code.len(), 4);
        assert_eq!(module.constants[0], Value::Integer(42));
        assert_eq!(module.strings[0], "x");
    }

    #[test]
    fn test_compile_define_function() {
        let source = "(define (add1 x) (+ x 1))";
        let ast = parse(source).unwrap();
        let env = Rc::new(Environment::new());
        let module = compile(&ast, source.to_string(), env).unwrap();

        // Should have: LoadConst(procedure), DefineVar, LoadConst(unspecified), Return
        assert_eq!(module.code.len(), 4);
        assert_eq!(module.strings[0], "add1");
        match &module.constants[0] {
            Value::Procedure { .. } => {} // Success
            _ => panic!("Expected procedure constant"),
        }
    }
}
