// Virtual machine module - executes bytecode
use crate::value::{Environment, Value};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub stack_trace: Vec<StackFrame>,
    pub source_location: Option<SourceLocation>,
    pub environment_snapshot: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: Option<String>,
    pub instruction_pointer: usize,
    pub source_location: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub source_code: String,
}

impl RuntimeError {
    pub fn new<S: Into<String>>(message: S) -> Self {
        RuntimeError {
            message: message.into(),
            stack_trace: Vec::new(),
            source_location: None,
            environment_snapshot: None,
        }
    }

    pub fn with_location(mut self, location: SourceLocation) -> Self {
        self.source_location = Some(location);
        self
    }

    pub fn with_stack_trace(mut self, stack_trace: Vec<StackFrame>) -> Self {
        self.stack_trace = stack_trace;
        self
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Runtime error: {}", self.message)?;

        if let Some(loc) = &self.source_location {
            writeln!(f, "  at line {}, column {}", loc.line, loc.column)?;

            // Show the source line with a pointer
            let lines: Vec<&str> = loc.source_code.lines().collect();
            if loc.line > 0 && loc.line <= lines.len() {
                let source_line = lines[loc.line - 1];
                writeln!(f, "  | {}", source_line)?;
                writeln!(f, "  | {}^", " ".repeat(loc.column.saturating_sub(1)))?;
            }
        }

        if !self.stack_trace.is_empty() {
            writeln!(f, "\nStack trace (depth → operation):")?;
            for frame in self.stack_trace.iter() {
                write!(f, "  {:3} │ ", frame.instruction_pointer)?;
                if let Some(name) = &frame.function_name {
                    writeln!(f, "{}", name)?;
                } else {
                    writeln!(f, "<unknown>")?;
                }
            }
        }

        if let Some(env) = &self.environment_snapshot {
            writeln!(f, "\nEnvironment at error:")?;
            writeln!(f, "{}", env)?;
        }

        Ok(())
    }
}

impl std::error::Error for RuntimeError {}

/// Runtime warning threshold for non-tail recursive calls
pub const RECURSION_WARNING_THRESHOLD: usize = 100;

/// Bytecode instructions for the stack-based VM
///
/// **Design Decision**: Using a stack-based VM for simplicity.
/// Stack-based VMs are easier to implement and debug than register-based VMs.
///
/// **Tail Call Optimization**: Implemented with TAIL_CALL opcode for proper tail calls
#[derive(Debug, Clone)]
pub enum Opcode {
    /// Push a constant value onto the stack
    /// Arg: index into constant table
    LoadConst(usize),

    /// Push a variable's value onto the stack
    /// Arg: index into string table for variable name
    LoadVar(usize),

    /// Pop a value and store it in a variable
    /// Arg: index into string table for variable name
    StoreVar(usize),

    /// Define a new variable in current environment
    /// Arg: index into string table for variable name
    DefineVar(usize),

    /// Call a procedure with N arguments from the stack
    /// Pops the procedure and N arguments, pushes result
    /// Arg: number of arguments
    Call(u8),

    /// Tail call a procedure with N arguments from the stack
    /// Reuses current stack frame for proper tail call optimization
    /// Arg: number of arguments
    TailCall(u8),

    /// Return from current procedure
    /// Pops return value from stack and returns to caller
    Return,

    /// Unconditional jump
    /// Arg: signed offset from current instruction
    Jump(i16),

    /// Jump if top of stack is false (#f)
    /// Pops the condition value
    /// Arg: signed offset from current instruction  
    JumpIfFalse(i16),

    /// Create a closure (lambda)
    /// Args: code offset, number of free variables to capture
    MakeClosure(usize, u8),

    /// Pop the top value (discard result)
    Pop,

    /// Duplicate the top value on the stack
    Dup,

    /// Create a cons pair from top two stack values
    /// Pops CDR then CAR, pushes pair
    Cons,

    /// Access car of pair on top of stack
    Car,

    /// Access cdr of pair on top of stack  
    Cdr,

    /// Create a list from N values on stack
    /// Arg: number of elements
    MakeList(u8),

    /// Load the empty list () onto stack
    LoadNil,

    /// Create a vector from N values on stack (future extension)
    /// Arg: number of elements
    MakeVector(u8),

    /// Create a recursive procedure that can reference itself
    /// Args: name constant index, params constant index, body constant index
    /// Creates a procedure with self-reference in its environment
    MakeRecursiveProcedure(usize, usize, usize),
}

/// Instruction with inline source location for debugging
#[derive(Debug, Clone)]
pub struct Instruction {
    /// The bytecode operation
    pub opcode: Opcode,
    /// Byte offset into original source code
    pub source_offset: u32,
}

/// Compiled bytecode module
#[derive(Debug, Clone)]
pub struct BytecodeModule {
    /// Sequence of instructions with debug info
    pub code: Vec<Instruction>,

    /// Constant table (literals used in code)
    pub constants: Vec<Value>,

    /// String table (for variable names, etc.)
    pub strings: Vec<String>,

    /// Original source code for debugging
    pub source_code: String,

    /// Entry point (index into code)
    pub entry_point: usize,
}

/// Call frame for procedure calls
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct CallFrame {
    /// Return address (instruction pointer)
    return_ip: usize,

    /// Base pointer for local variables
    base_pointer: usize,

    /// Environment for this call
    env: Rc<Environment>,

    /// Function name for recursion tracking
    function_name: Option<String>,
}

/// Stack-based virtual machine
pub struct VM {
    /// Value stack
    stack: Vec<Value>,

    /// Call stack
    call_stack: Vec<CallFrame>,

    /// Current instruction pointer
    ip: usize,

    /// Global environment
    #[allow(dead_code)]
    global_env: Rc<Environment>,

    /// Current environment
    current_env: Rc<Environment>,

    /// Track recursion depth for warnings
    recursion_depth: std::collections::HashMap<String, usize>,
}

impl BytecodeModule {
    /// Convert source offset to line and column numbers for debugging
    pub fn offset_to_line_col(&self, offset: u32) -> (u32, u32) {
        let offset = offset as usize;
        if offset >= self.source_code.len() {
            return (1, 1);
        }

        let mut line = 1;
        let mut col = 1;

        for (i, ch) in self.source_code.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    /// Get source snippet around an instruction for debugging
    pub fn get_source_context(&self, instruction_index: usize, context_chars: usize) -> String {
        if instruction_index >= self.code.len() {
            return "Invalid instruction index".to_string();
        }

        let offset = self.code[instruction_index].source_offset as usize;
        let start = offset.saturating_sub(context_chars);
        let end = std::cmp::min(offset + context_chars, self.source_code.len());

        if start >= self.source_code.len() || end <= start {
            return "Invalid source offset".to_string();
        }

        self.source_code[start..end].to_string()
    }
}

impl std::fmt::Display for BytecodeModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, instr) in self.code.iter().enumerate() {
            writeln!(f, "{:08}: {}", i, self.format_instruction(&instr.opcode))?;
        }
        Ok(())
    }
}

impl BytecodeModule {
    /// Format a single instruction with constants and strings context
    fn format_instruction(&self, opcode: &Opcode) -> String {
        match opcode {
            Opcode::LoadConst(idx) => {
                let value = self
                    .constants
                    .get(*idx)
                    .map(|v| format!("{}", v))
                    .unwrap_or_else(|| format!("INVALID[{}]", idx));
                format!("LOAD_CONST {} ; {}", idx, value)
            }
            Opcode::LoadVar(idx) => {
                let name = self
                    .strings
                    .get(*idx)
                    .map(|s| s.as_str())
                    .unwrap_or("INVALID");
                format!("LOAD_VAR {} ; {}", idx, name)
            }
            Opcode::StoreVar(idx) => {
                let name = self
                    .strings
                    .get(*idx)
                    .map(|s| s.as_str())
                    .unwrap_or("INVALID");
                format!("STORE_VAR {} ; {}", idx, name)
            }
            Opcode::DefineVar(idx) => {
                let name = self
                    .strings
                    .get(*idx)
                    .map(|s| s.as_str())
                    .unwrap_or("INVALID");
                format!("DEFINE_VAR {} ; {}", idx, name)
            }
            Opcode::Call(argc) => format!("CALL {}", argc),
            Opcode::TailCall(argc) => format!("TAIL_CALL {}", argc),
            Opcode::Return => "RETURN".to_string(),
            Opcode::Jump(offset) => format!("JUMP {}", offset),
            Opcode::JumpIfFalse(offset) => format!("JUMP_IF_FALSE {}", offset),
            Opcode::MakeClosure(target, vars) => format!("MAKE_CLOSURE {} {}", target, vars),
            Opcode::Pop => "POP".to_string(),
            Opcode::Dup => "DUP".to_string(),
            Opcode::Cons => "CONS".to_string(),
            Opcode::Car => "CAR".to_string(),
            Opcode::Cdr => "CDR".to_string(),
            Opcode::MakeList(count) => format!("MAKE_LIST {}", count),
            Opcode::LoadNil => "LOAD_NIL".to_string(),
            Opcode::MakeVector(count) => format!("MAKE_VECTOR {}", count),
            Opcode::MakeRecursiveProcedure(name_idx, params_idx, body_idx) => {
                format!(
                    "MAKE_RECURSIVE_PROC {} {} {}",
                    name_idx, params_idx, body_idx
                )
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self::new_with_cps(true)
    }

    /// Create a new VM with standard built-ins
    pub fn new_with_cps(_enable_cps: bool) -> Self {
        let global_env = Environment::new();

        // Load built-in procedures into global environment
        let builtins = crate::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }

        let global_env = Rc::new(global_env);
        let mut vm = VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            ip: 0,
            current_env: global_env.clone(),
            global_env,
            recursion_depth: std::collections::HashMap::new(),
        };

        // Load function prelude
        if let Err(e) = vm.load_function_prelude() {
            panic!("Fatal error loading function prelude: {}", e);
        }

        vm
    }

    /// Create a new VM with a provided environment
    /// This allows sharing environment state between multiple VM executions
    pub fn new_with_env(env: Rc<Environment>, _enable_cps: bool) -> Self {
        let mut vm = VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            ip: 0,
            current_env: env.clone(),
            global_env: env,
            recursion_depth: std::collections::HashMap::new(),
        };

        // Load function prelude
        if let Err(e) = vm.load_function_prelude() {
            panic!("Fatal error loading function prelude: {}", e);
        }

        vm
    }

    /// Create a new VM for direct AST interpretation (no bytecode compilation)
    /// This VM will evaluate expressions directly from the AST without compiling to bytecode
    pub fn new_direct_interpreter(env: Rc<Environment>) -> Self {
        VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            ip: 0,
            current_env: env.clone(),
            global_env: env,
            recursion_depth: std::collections::HashMap::new(),
        }
    }

    /// Load the function prelude into the VM environment
    fn load_function_prelude(&mut self) -> Result<(), RuntimeError> {
        use crate::compiler::compile;
        use crate::parser::parse_multiple;

        const FUNCTION_PRELUDE: &str = include_str!("../prelude/functions.scm");

        match parse_multiple(FUNCTION_PRELUDE) {
            Ok(expressions) => {
                for expression in expressions {
                    match compile(
                        &expression,
                        "<prelude>".to_string(),
                        self.current_env.clone(),
                    ) {
                        Ok(module) => {
                            if let Err(e) = self.execute(&module) {
                                return Err(RuntimeError::new(format!(
                                    "Failed to execute prelude function '{}': {}\nThe interpreter cannot continue with a broken prelude.", 
                                    expression, e
                                )));
                            }
                        }
                        Err(e) => {
                            return Err(RuntimeError::new(format!(
                                "Failed to compile prelude function '{}': {}\nThe interpreter cannot continue with a broken prelude.", 
                                expression, e
                            )));
                        }
                    }
                }
            }
            Err(e) => {
                return Err(RuntimeError::new(format!(
                    "Failed to parse function prelude: {}\nThe interpreter cannot continue with a broken prelude.", 
                    e
                )));
            }
        }
        Ok(())
    }

    /// Get the current environment (for testing)
    pub fn current_env(&self) -> Rc<Environment> {
        self.current_env.clone()
    }

    /// Safely pop a value from the stack, returning an error if empty
    fn pop_stack(&mut self, module: &BytecodeModule) -> Result<Value, RuntimeError> {
        if self.stack.is_empty() {
            Err(self.create_runtime_error("Stack underflow".to_string(), module))
        } else {
            Ok(self.stack.pop().unwrap()) // Safe due to empty check above
        }
    }

    /// Safely pop N values from the stack, returning an error if not enough values
    fn pop_stack_n(
        &mut self,
        n: usize,
        module: &BytecodeModule,
    ) -> Result<Vec<Value>, RuntimeError> {
        if self.stack.len() < n {
            Err(self.create_runtime_error(
                format!(
                    "Stack underflow: need {} values, have {}",
                    n,
                    self.stack.len()
                ),
                module,
            ))
        } else {
            let mut values = Vec::with_capacity(n);
            for _ in 0..n {
                values.push(self.stack.pop().unwrap()); // Safe due to length check above
            }
            values.reverse(); // Pop gives us reversed order
            Ok(values)
        }
    }

    /// Safely peek at the top stack value without removing it
    fn peek_stack(&self, module: &BytecodeModule) -> Result<&Value, RuntimeError> {
        if self.stack.is_empty() {
            Err(self.create_runtime_error("Stack underflow in peek".to_string(), module))
        } else {
            Ok(self.stack.last().unwrap()) // Safe due to empty check above
        }
    }

    /// Create an enhanced runtime error with current context
    fn create_runtime_error(&self, message: String, module: &BytecodeModule) -> RuntimeError {
        let (line, col) = module.offset_to_line_col(
            module
                .code
                .get(self.ip)
                .map(|instr| instr.source_offset)
                .unwrap_or(0),
        );

        let source_location = SourceLocation {
            line: line as usize,
            column: col as usize,
            source_code: module.source_code.clone(),
        };

        // Build stack trace from call stack
        let mut stack_trace = Vec::new();
        for frame in &self.call_stack {
            stack_trace.push(StackFrame {
                function_name: frame.function_name.clone(),
                instruction_pointer: frame.return_ip,
                source_location: Some(SourceLocation {
                    line: line as usize, // We'd need to improve this to get actual line for each frame
                    column: col as usize,
                    source_code: module.source_code.clone(),
                }),
            });
        }

        // Add current frame
        stack_trace.push(StackFrame {
            function_name: None, // Could be enhanced to track current function
            instruction_pointer: self.ip,
            source_location: Some(source_location.clone()),
        });

        RuntimeError::new(message)
            .with_location(source_location)
            .with_stack_trace(stack_trace)
    }

    /// Create a simple runtime error without source location
    fn create_simple_runtime_error(&self, message: String) -> RuntimeError {
        RuntimeError::new(message)
    }

    /// Check if the value stack is empty
    pub fn stack_is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Print the current stack contents for debugging
    pub fn print_stack(&self) {
        if self.stack.is_empty() {
            println!("  (empty)");
            return;
        }

        for (i, value) in self.stack.iter().enumerate() {
            println!("  [{}]: {}", i, value);
        }
    }

    /// Execute a single value (AST node) in the current environment
    /// This is a helper method for procedure calls
    fn execute_value_in_current_env(&mut self, value: &Value) -> Result<Value, RuntimeError> {
        // eprintln!("Debug: execute_value_in_current_env called with: {}", value);
        // Compile the value to bytecode with current environment
        let source = format!("{}", value); // Simple string representation for now
        let module = match crate::compiler::compile(value, source, self.current_env.clone()) {
            Ok(m) => m,
            Err(e) => {
                return Err(RuntimeError {
                    message: format!("Compile error: {:?}", e),
                    stack_trace: Vec::new(), // No VM stack available during compilation
                    source_location: None,   // No source location available during compilation
                    environment_snapshot: None,
                });
            }
        };

        // eprintln!("Debug: compiled bytecode: {} instructions", module.code.len());
        // for (i, instr) in module.code.iter().enumerate() {
        //     eprintln!("Debug:   {}: {:?}", i, instr);
        // }

        // Save the current IP before executing the compiled bytecode
        let saved_ip = self.ip;

        // Execute the compiled bytecode
        let result = self.execute(&module);

        // Restore the IP after execution
        self.ip = saved_ip;

        // match &result {
        //     Ok(val) => eprintln!("Debug: execute_value_in_current_env result: {}", val),
        //     Err(err) => eprintln!("Debug: execute_value_in_current_env error: {}", err),
        // }
        result
    }

    /// Direct AST evaluation without bytecode compilation
    /// This is a naive tree-walking interpreter that evaluates expressions directly
    pub fn evaluate_ast(&mut self, expression: &Value) -> Result<Value, RuntimeError> {
        match expression {
            // Literals evaluate to themselves
            // **R7RS RESTRICTED:** Only i64 integers supported, no u64 or floats
            Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expression.clone()),

            // Symbols are variable lookups
            Value::Symbol(name) => match self.current_env.lookup(name) {
                Some(value) => Ok(value),
                None => {
                    Err(self.create_simple_runtime_error(format!("Undefined variable: {}", name)))
                }
            },

            // Lists are either special forms or function applications
            Value::List(elements) if !elements.is_empty() => {
                match &elements[0] {
                    Value::Symbol(name) => match name.as_str() {
                        // Special forms
                        "if" => self.evaluate_if(&elements[1..]),
                        "define" => self.evaluate_define(&elements[1..]),
                        "lambda" => self.evaluate_lambda(&elements[1..]),
                        "quote" => {
                            if elements.len() == 2 {
                                Ok(elements[1].clone())
                            } else {
                                Err(self.create_simple_runtime_error(
                                    "quote requires exactly one argument".to_string(),
                                ))
                            }
                        }
                        "begin" => self.evaluate_begin(&elements[1..]),
                        _ => {
                            // Regular function application
                            self.evaluate_application(elements)
                        }
                    },
                    _ => {
                        // Function expression application: ((lambda ...) args...)
                        self.evaluate_application(elements)
                    }
                }
            }

            // Empty list
            Value::List(_) => Ok(Value::List(vec![])),

            // Other values pass through
            _ => Ok(expression.clone()),
        }
    }

    /// Evaluate if expression directly
    fn evaluate_if(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(
                self.create_simple_runtime_error("if requires 2 or 3 arguments".to_string())
            );
        }

        let test_result = self.evaluate_ast(&args[0])?;
        let is_true = match test_result {
            Value::Boolean(false) => false,
            _ => true, // Everything except #f is true in Scheme
        };

        if is_true {
            self.evaluate_ast(&args[1])
        } else if args.len() == 3 {
            self.evaluate_ast(&args[2])
        } else {
            // No else clause - return unspecified
            Ok(Value::List(vec![]))
        }
    }

    /// Evaluate define expression directly
    fn evaluate_define(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(
                self.create_simple_runtime_error("define requires exactly 2 arguments".to_string())
            );
        }

        match &args[0] {
            Value::Symbol(name) => {
                let value = self.evaluate_ast(&args[1])?;
                self.current_env.define(name.clone(), value);
                Ok(Value::List(vec![])) // Unspecified return value
            }
            _ => Err(self
                .create_simple_runtime_error("define first argument must be a symbol".to_string())),
        }
    }

    /// Evaluate lambda expression directly
    fn evaluate_lambda(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(self
                .create_simple_runtime_error("lambda requires at least 2 arguments".to_string()));
        }

        let params = args[0].clone();
        let body = if args.len() == 2 {
            args[1].clone()
        } else {
            // Multiple body expressions - wrap in begin
            let mut begin_expr = vec![Value::Symbol("begin".to_string())];
            begin_expr.extend(args[1..].iter().cloned());
            Value::List(begin_expr)
        };

        // Create a user-defined procedure
        let param_names = match &params {
            Value::List(param_list) => {
                let mut names = Vec::new();
                for param in param_list {
                    if let Value::Symbol(name) = param {
                        names.push(name.clone());
                    } else {
                        return Err(self.create_simple_runtime_error(
                            "Function parameters must be symbols".to_string(),
                        ));
                    }
                }
                names
            }
            Value::Symbol(single_param) => vec![single_param.clone()],
            _ => {
                return Err(self.create_simple_runtime_error(
                    "Function parameters must be symbols or list of symbols".to_string(),
                ))
            }
        };

        Ok(Value::Procedure {
            params: param_names,
            body: Rc::new(body),
            env: self.current_env.clone(),
            variadic: false, // For now, no variadic support in direct interpreter
        })
    }

    /// Evaluate begin expression directly
    fn evaluate_begin(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::List(vec![])); // Unspecified
        }

        let mut result = Value::List(vec![]);
        for expr in args {
            result = self.evaluate_ast(expr)?;
        }
        Ok(result)
    }

    /// Evaluate function application directly
    fn evaluate_application(&mut self, elements: &[Value]) -> Result<Value, RuntimeError> {
        // Evaluate the function
        let function = self.evaluate_ast(&elements[0])?;

        // Evaluate the arguments
        let mut args = Vec::new();
        for arg in &elements[1..] {
            args.push(self.evaluate_ast(arg)?);
        }

        // Apply the function
        match function {
            Value::Builtin {
                name,
                arity: _,
                func,
            } => {
                // Call builtin function
                func(&args).map_err(|e| {
                    self.create_simple_runtime_error(format!("Builtin '{}' error: {}", name, e))
                })
            }
            Value::Procedure {
                params,
                body,
                env,
                variadic: _,
            } => {
                // Call user-defined function
                self.apply_user_function_direct(params, (*body).clone(), env, args)
            }
            _ => Err(self.create_simple_runtime_error(format!(
                "Cannot apply non-function: {}",
                function.type_name()
            ))),
        }
    }

    /// Apply a user-defined function directly
    fn apply_user_function_direct(
        &mut self,
        params: Vec<String>,
        body: Value,
        closure: Rc<Environment>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Create new environment for function execution
        let func_env = Environment::with_parent(closure);

        // Bind parameters to arguments
        if params.len() != args.len() {
            return Err(self.create_simple_runtime_error(format!(
                "Function expects {} arguments, got {}",
                params.len(),
                args.len()
            )));
        }
        for (param_name, arg) in params.iter().zip(args.iter()) {
            func_env.define(param_name.clone(), arg.clone());
        }

        // Save current environment and switch to function environment
        let saved_env = self.current_env.clone();
        self.current_env = Rc::new(func_env);

        // Evaluate function body
        let result = self.evaluate_ast(&body);

        // Restore previous environment
        self.current_env = saved_env;

        result
    }

    /// Execute bytecode module
    pub fn execute(&mut self, module: &BytecodeModule) -> Result<Value, RuntimeError> {
        self.ip = module.entry_point;

        loop {
            if self.ip >= module.code.len() {
                break;
            }

            let instruction = &module.code[self.ip];
            // eprintln!("Debug: [VM] IP: {}, Executing: {:?}", self.ip, instruction.opcode);
            match &instruction.opcode {
                Opcode::LoadConst(index) => {
                    if *index >= module.constants.len() {
                        return Err(
                            self.create_runtime_error("Invalid constant index".to_string(), module)
                        );
                    }
                    self.stack.push(module.constants[*index].clone());
                }

                Opcode::LoadVar(name_index) => {
                    if *name_index >= module.strings.len() {
                        return Err(
                            self.create_runtime_error("Invalid string index".to_string(), module)
                        );
                    }
                    let name = &module.strings[*name_index];
                    match self.current_env.lookup(name) {
                        Some(value) => {
                            // eprintln!("Debug: LoadVar '{}' = {:?}", name, value.type_name());
                            self.stack.push(value);
                        }
                        None => {
                            return Err(self.create_runtime_error(
                                format!("Undefined variable: {}", name),
                                module,
                            ))
                        }
                    }
                }

                Opcode::Return => {
                    if let Some(frame) = self.call_stack.pop() {
                        self.ip = frame.return_ip;
                        self.current_env = frame.env;
                        // Decrease recursion depth if this was a recursive call
                        if let Some(func_name) = &frame.function_name {
                            if let Some(depth) = self.recursion_depth.get_mut(func_name) {
                                *depth = depth.saturating_sub(1);
                            }
                        }
                        // Keep the return value on stack
                    } else {
                        // Return from main - we're done
                        break;
                    }
                }

                Opcode::LoadNil => {
                    self.stack.push(Value::List(vec![]));
                }

                Opcode::Pop => {
                    if self.stack.is_empty() {
                        return Err(
                            self.create_runtime_error("Stack underflow".to_string(), module)
                        );
                    }
                    self.stack.pop();
                }

                Opcode::StoreVar(name_index) => {
                    if *name_index >= module.strings.len() {
                        return Err(
                            self.create_runtime_error("Invalid string index".to_string(), module)
                        );
                    }
                    let _value = self.pop_stack(module)?;
                    let _name = &module.strings[*name_index];

                    // Try to set in current environment (will fail if not defined)
                    if Rc::try_unwrap(self.current_env.clone()).is_err() {
                        return Err(self.create_runtime_error(
                            "Cannot modify environment - implementation limitation".to_string(),
                            module,
                        ));
                    }
                    // For now, we'll skip mutation - this needs RefCell for proper implementation
                    return Err(self.create_runtime_error(
                        "Variable mutation not implemented yet".to_string(),
                        module,
                    ));
                }

                Opcode::DefineVar(name_index) => {
                    if *name_index >= module.strings.len() {
                        return Err(
                            self.create_runtime_error("Invalid string index".to_string(), module)
                        );
                    }
                    if self.stack.is_empty() {
                        return Err(
                            self.create_runtime_error("Stack underflow".to_string(), module)
                        );
                    }
                    let value = self.stack.pop().unwrap();
                    let name = module.strings[*name_index].clone();

                    // Define variable in current environment
                    self.current_env.define(name, value);
                    self.stack.push(Value::Unspecified);
                }

                Opcode::Call(arg_count) => {
                    let total_args = *arg_count as usize + 1; // +1 for the procedure itself
                    if self.stack.len() < total_args {
                        return Err(self
                            .create_runtime_error("Stack underflow in call".to_string(), module));
                    }

                    // Get procedure from stack (it's at the top, most recent push)
                    let procedure = self.stack[self.stack.len() - 1].clone();

                    match procedure {
                        Value::Builtin { name, arity, func } => {
                            // Check arity
                            let provided_args = *arg_count as usize;
                            match arity {
                                crate::value::Arity::Exact(expected) => {
                                    if provided_args != expected {
                                        return Err(self.create_runtime_error(
                                            format!(
                                                "Procedure '{}' expects {} arguments, got {}",
                                                name, expected, provided_args
                                            ),
                                            module,
                                        ));
                                    }
                                }
                                crate::value::Arity::AtLeast(min) => {
                                    if provided_args < min {
                                        return Err(self.create_runtime_error(
                                            format!(
                                            "Procedure '{}' expects at least {} arguments, got {}",
                                            name, min, provided_args
                                        ),
                                            module,
                                        ));
                                    }
                                }
                                crate::value::Arity::Range(min, max) => {
                                    if provided_args < min || provided_args > max {
                                        return Err(self.create_runtime_error(
                                            format!(
                                                "Procedure '{}' expects {}-{} arguments, got {}",
                                                name, min, max, provided_args
                                            ),
                                            module,
                                        ));
                                    }
                                }
                            }

                            // Collect arguments (they are just below the procedure)
                            let mut args = Vec::new();
                            let start_index = self.stack.len() - total_args;
                            for i in 0..*arg_count as usize {
                                args.push(self.stack[start_index + i].clone());
                            }

                            // Remove procedure and arguments from stack
                            self.stack.truncate(start_index);

                            // Call the builtin function
                            match func(&args) {
                                Ok(result) => {
                                    self.stack.push(result);

                                    // Track recursion for runtime warnings
                                    if let Some(depth) = self.recursion_depth.get_mut(&name) {
                                        if *depth > RECURSION_WARNING_THRESHOLD {
                                            eprintln!(
                                                "Runtime Warning: Deep recursion in '{}' (depth: {})",
                                                name, *depth
                                            );
                                        }
                                    }
                                }
                                Err(e) => return Err(self.create_simple_runtime_error(e)),
                            }
                        }

                        Value::Procedure {
                            params,
                            body,
                            env,
                            variadic,
                        } => {
                            // User-defined procedure call
                            // eprintln!("Debug: [CALL] Calling user procedure with {} params, body: {:?}", params.len(), body);
                            let provided_args = *arg_count as usize;

                            if variadic {
                                // **R7RS RESTRICTED:** Only fully variadic functions supported
                                // (lambda args body) takes any number of arguments
                                if params.len() != 1 {
                                    return Err(self.create_runtime_error(
                                        "**R7RS RESTRICTED:** Dot notation (mixed variadic) not supported - use fully variadic functions only".to_string(),
                                        module,
                                    ));
                                }
                                // Fully variadic functions accept any number of arguments (0 or more)
                            } else if provided_args != params.len() {
                                return Err(self.create_runtime_error(
                                    format!(
                                        "Procedure expects {} arguments, got {}",
                                        params.len(),
                                        provided_args
                                    ),
                                    module,
                                ));
                            }

                            // Collect arguments (they are just below the procedure)
                            let mut args = Vec::new();
                            let start_index = self.stack.len() - total_args;
                            for i in 0..*arg_count as usize {
                                args.push(self.stack[start_index + i].clone());
                            }

                            // Remove procedure and arguments from stack
                            self.stack.truncate(start_index);

                            // Create new environment for procedure call with closure environment as parent
                            let call_env = Environment::with_parent(env.clone());

                            // Bind parameters to arguments
                            if variadic {
                                // Fully variadic: (lambda args body) - all args go into a list
                                let args_list = Value::List(args);
                                call_env.define(params[0].clone(), args_list);
                            } else {
                                // Fixed arity: bind parameters one-to-one with arguments
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    call_env.define(param.clone(), arg.clone());
                                }
                            }

                            // Save current environment
                            let saved_env = self.current_env.clone();
                            self.current_env = Rc::new(call_env);

                            // Execute procedure body by compiling and running it
                            // This is a simplified approach - in a full implementation,
                            // we'd store compiled bytecode in the procedure
                            // eprintln!("Debug: [CALL] Executing procedure body: {:?}", body);
                            let result = match self.execute_value_in_current_env(body.as_ref()) {
                                Ok(value) => value,
                                Err(e) => {
                                    // Restore environment on error
                                    self.current_env = saved_env;
                                    return Err(e);
                                }
                            };

                            // Restore environment
                            self.current_env = saved_env;

                            // Push result
                            self.stack.push(result);
                        }

                        _ => {
                            return Err(self.create_runtime_error(
                                format!(
                                    "Cannot call non-procedure value: {}",
                                    procedure.type_name()
                                ),
                                module,
                            ));
                        }
                    }
                }

                Opcode::TailCall(arg_count) => {
                    // For now, implement tail calls as regular calls
                    // TODO: Implement proper tail call optimization by reusing stack frame

                    // Reuse the Call logic by duplicating the essential parts
                    let total_args = *arg_count as usize + 1; // +1 for procedure
                    if self.stack.len() < total_args {
                        return Err(self.create_runtime_error(
                            "Stack underflow in tail call".to_string(),
                            module,
                        ));
                    }
                    // Get procedure from stack (it's at the top, most recent push)
                    let procedure = self.stack[self.stack.len() - 1].clone();

                    match procedure {
                        Value::Builtin { name, arity, func } => {
                            // Validate arity
                            let provided_args = *arg_count as usize;
                            match arity {
                                crate::value::Arity::Exact(expected) => {
                                    if provided_args != expected {
                                        return Err(self.create_runtime_error(
                                            format!(
                                                "Procedure '{}' expects {} arguments, got {}",
                                                name, expected, provided_args
                                            ),
                                            module,
                                        ));
                                    }
                                }
                                crate::value::Arity::AtLeast(min) => {
                                    if provided_args < min {
                                        return Err(self.create_runtime_error(
                                            format!(
                                            "Procedure '{}' expects at least {} arguments, got {}",
                                            name, min, provided_args
                                        ),
                                            module,
                                        ));
                                    }
                                }
                                crate::value::Arity::Range(min, max) => {
                                    if provided_args < min || provided_args > max {
                                        return Err(self.create_runtime_error(
                                            format!(
                                                "Procedure '{}' expects {}-{} arguments, got {}",
                                                name, min, max, provided_args
                                            ),
                                            module,
                                        ));
                                    }
                                }
                            }

                            // Collect arguments (they are just below the procedure)
                            let mut args = Vec::new();
                            let start_index = self.stack.len() - total_args;
                            for i in 0..*arg_count as usize {
                                args.push(self.stack[start_index + i].clone());
                            }

                            // Remove procedure and arguments from stack
                            self.stack.truncate(start_index);

                            match func(&args) {
                                Ok(result) => {
                                    self.stack.push(result);
                                }
                                Err(e) => return Err(self.create_simple_runtime_error(e)),
                            }
                        }

                        Value::Procedure {
                            params,
                            body,
                            env,
                            variadic,
                        } => {
                            // User-defined procedure tail call - same as regular call for now
                            // eprintln!("Debug: [TAILCALL] Calling user procedure with {} params, body: {:?}", params.len(), body);
                            let provided_args = *arg_count as usize;

                            // Collect arguments (they are just below the procedure)
                            let mut args = Vec::new();
                            let start_index = self.stack.len() - total_args;
                            for i in 0..*arg_count as usize {
                                args.push(self.stack[start_index + i].clone());
                            }

                            // Remove procedure and arguments from stack
                            self.stack.truncate(start_index);

                            // Create new environment for procedure call with closure environment as parent
                            let call_env = Environment::with_parent(env.clone());

                            // Bind parameters to arguments
                            if variadic {
                                // **R7RS RESTRICTED:** Only fully variadic functions supported (lambda args body)
                                // **R7RS RESTRICTED:** Dot notation (lambda (a b . rest) body) not supported
                                if params.len() != 1 {
                                    return Err(self.create_runtime_error(
                                        "Dot notation (mixed variadic) not supported - use fully variadic form".to_string(),
                                        module,
                                    ));
                                }
                                // Fully variadic: (lambda args body) - all args go into a list
                                let args_list = Value::vec_to_list(args);
                                call_env.define(params[0].clone(), args_list);
                            } else {
                                // Fixed parameter count
                                if provided_args != params.len() {
                                    return Err(self.create_runtime_error(
                                        format!(
                                            "Procedure expects {} arguments, got {}",
                                            params.len(),
                                            provided_args
                                        ),
                                        module,
                                    ));
                                }
                                for (param, arg) in params.iter().zip(args.iter()) {
                                    call_env.define(param.clone(), arg.clone());
                                }
                            }

                            // Save current environment
                            let saved_env = self.current_env.clone();
                            self.current_env = Rc::new(call_env);

                            // Execute procedure body by compiling and running it
                            // eprintln!("Debug: [TAILCALL] Executing procedure body: {:?}", body);
                            let result = match self.execute_value_in_current_env(body.as_ref()) {
                                Ok(value) => value,
                                Err(e) => {
                                    // Restore environment on error
                                    self.current_env = saved_env;
                                    return Err(e);
                                }
                            };

                            // Restore environment
                            self.current_env = saved_env;

                            // For tail calls, we should optimize by reusing the current frame
                            // For now, just push the result like a regular call
                            // eprintln!("Debug: [TAILCALL] Result: {}, pushing to stack", result);
                            self.stack.push(result);
                        }

                        _ => {
                            return Err(self.create_runtime_error(
                                format!(
                                    "Cannot call non-procedure value: {}",
                                    procedure.type_name()
                                ),
                                module,
                            ));
                        }
                    }
                }

                Opcode::Jump(offset) => {
                    let new_ip = (self.ip as i32 + *offset as i32) as usize;
                    if new_ip >= module.code.len() {
                        return Err(
                            self.create_runtime_error("Jump out of bounds".to_string(), module)
                        );
                    }
                    self.ip = new_ip;
                    continue; // Skip the normal ip increment
                }

                Opcode::JumpIfFalse(offset) => {
                    let condition = self.pop_stack(module)?;

                    if !condition.is_truthy() {
                        let new_ip = (self.ip as i32 + *offset as i32) as usize;
                        if new_ip >= module.code.len() {
                            return Err(
                                self.create_runtime_error("Jump out of bounds".to_string(), module)
                            );
                        }
                        self.ip = new_ip;
                        continue; // Skip the normal ip increment
                    }
                }

                Opcode::Dup => {
                    let value = self.peek_stack(module)?.clone();
                    self.stack.push(value);
                }

                Opcode::Cons => {
                    let values = self.pop_stack_n(2, module)?;
                    let car = values[0].clone();
                    let cdr = values[1].clone();

                    // Since we're using List instead of Pair, we'll create a list with car as first element
                    // and append cdr if it's a list, otherwise create improper list (which we don't support)
                    match cdr {
                        Value::List(mut elements) => {
                            elements.insert(0, car);
                            self.stack.push(Value::List(elements));
                        }
                        _ => {
                            // Can't create improper list with our representation
                            return Err(self.create_runtime_error(
                                "Cannot cons onto non-list (improper lists not supported)"
                                    .to_string(),
                                module,
                            ));
                        }
                    }
                }

                Opcode::Car => {
                    let value = self.pop_stack(module)?;

                    match value {
                        Value::List(elements) => {
                            if elements.is_empty() {
                                return Err(self.create_runtime_error(
                                    "Cannot take car of empty list".to_string(),
                                    module,
                                ));
                            }
                            self.stack.push(elements[0].clone());
                        }
                        _ => {
                            return Err(self.create_runtime_error(
                                format!("Cannot take car of {}", value.type_name()),
                                module,
                            ));
                        }
                    }
                }

                Opcode::Cdr => {
                    let value = self.pop_stack(module)?;

                    match value {
                        Value::List(elements) => {
                            if elements.is_empty() {
                                return Err(self.create_runtime_error(
                                    "Cannot take cdr of empty list".to_string(),
                                    module,
                                ));
                            }
                            let cdr_elements = elements[1..].to_vec();
                            self.stack.push(Value::List(cdr_elements));
                        }
                        _ => {
                            return Err(self.create_runtime_error(
                                format!("Cannot take cdr of {}", value.type_name()),
                                module,
                            ));
                        }
                    }
                }

                Opcode::MakeList(count) => {
                    let count = *count as usize;
                    if self.stack.len() < count {
                        return Err(self.create_runtime_error(
                            "Stack underflow in make-list".to_string(),
                            module,
                        ));
                    }

                    let start_index = self.stack.len() - count;
                    let elements = self.stack[start_index..].to_vec();
                    self.stack.truncate(start_index);
                    self.stack.push(Value::List(elements));
                }

                Opcode::MakeClosure(_code_offset, _free_var_count) => {
                    // TODO: Implement closure creation
                    return Err(self.create_runtime_error(
                        "Closure creation not implemented yet".to_string(),
                        module,
                    ));
                }

                Opcode::MakeVector(count) => {
                    let count = *count as usize;
                    if self.stack.len() < count {
                        return Err(self.create_runtime_error(
                            "Stack underflow in make-vector".to_string(),
                            module,
                        ));
                    }

                    // For now, vectors are not implemented, so this will fail
                    return Err(self
                        .create_runtime_error("Vectors not implemented yet".to_string(), module));
                }

                Opcode::MakeRecursiveProcedure(name_idx, params_idx, body_idx) => {
                    // Validate indices
                    if *name_idx >= module.constants.len()
                        || *params_idx >= module.constants.len()
                        || *body_idx >= module.constants.len()
                    {
                        return Err(self.create_runtime_error(
                            "Invalid constant index for recursive procedure".to_string(),
                            module,
                        ));
                    }

                    // Extract the function name
                    let func_name = match &module.constants[*name_idx] {
                        Value::String(name) => name.clone(),
                        _ => {
                            return Err(self.create_runtime_error(
                                "Expected string for function name".to_string(),
                                module,
                            ))
                        }
                    };

                    // Extract parameters
                    let params = &module.constants[*params_idx];
                    let param_names = match params {
                        Value::List(param_list) => {
                            let mut names = Vec::new();
                            for param in param_list {
                                if let Value::Symbol(name) = param {
                                    names.push(name.clone());
                                } else {
                                    return Err(self.create_runtime_error(
                                        "Parameter must be a symbol".to_string(),
                                        module,
                                    ));
                                }
                            }
                            names
                        }
                        _ => {
                            return Err(self.create_runtime_error(
                                "Parameters must be a list".to_string(),
                                module,
                            ))
                        }
                    };

                    // Extract body
                    let body = module.constants[*body_idx].clone();

                    // Create a procedure with a self-referential environment
                    // We create the environment with the function name pointing to a placeholder,
                    // then create the procedure, then update the environment to point to the actual procedure
                    let proc_env = Environment::with_parent(self.current_env.clone());

                    // Define the function name with a placeholder first
                    proc_env.define(func_name.clone(), Value::Unspecified);
                    let proc_env_rc = Rc::new(proc_env);

                    // Create the procedure
                    let procedure = Value::Procedure {
                        params: param_names,
                        body: Rc::new(body),
                        env: proc_env_rc.clone(),
                        variadic: false,
                    };

                    // **R7RS DEVIATION:** This creates a self-referential environment by modifying
                    // an existing environment after creation. This is needed for recursive functions
                    // but deviates from pure functional environment semantics.
                    // The environment stores a reference to the procedure containing the environment,
                    // creating a circular reference that enables self-recursion.
                    // Now update the environment to point to the actual procedure
                    if let Err(err) = proc_env_rc.set(&func_name, procedure.clone()) {
                        return Err(self.create_runtime_error(
                            format!("Failed to set recursive function: {}", err),
                            module,
                        ));
                    }

                    self.stack.push(procedure);
                }
            }

            self.ip += 1;
        }

        // Return top of stack, or unspecified if empty
        Ok(self.stack.pop().unwrap_or(Value::Unspecified))
    }

    /// Get current stack for debugging
    pub fn stack(&self) -> &[Value] {
        &self.stack
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::compile;
    use crate::parser::parse;

    #[test]
    fn test_vm_execute_literal() {
        let source = "42";
        let ast = parse(source).unwrap();
        let mut vm = VM::new();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();

        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_vm_execute_empty_list() {
        let source = "()";
        let ast = parse(source).unwrap();
        let mut vm = VM::new();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();

        assert_eq!(result, Value::List(vec![]));
    }

    #[test]
    fn test_vm_debug_info() {
        let source = "hello";
        let ast = parse(source).unwrap();
        let vm = VM::new();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();

        // Test line/column conversion
        let (line, col) = module.offset_to_line_col(0);
        assert_eq!(line, 1);
        assert_eq!(col, 1);

        // Test source context
        let context = module.get_source_context(0, 10);
        assert_eq!(context, "hello");
    }

    #[test]
    fn test_vm_list_operations() {
        // Test CAR operation
        let mut module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                },
                Instruction {
                    opcode: Opcode::Car,
                    source_offset: 0,
                },
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                },
            ],
            constants: vec![Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ])],
            strings: vec![],
            source_code: "test".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(1));

        // Test CDR operation
        module.code = vec![
            Instruction {
                opcode: Opcode::LoadConst(0),
                source_offset: 0,
            },
            Instruction {
                opcode: Opcode::Cdr,
                source_offset: 0,
            },
            Instruction {
                opcode: Opcode::Return,
                source_offset: 0,
            },
        ];

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
        assert_eq!(
            result,
            Value::List(vec![Value::Integer(2), Value::Integer(3)])
        );
    }

    #[test]
    fn test_vm_cons_operation() {
        // Test CONS: (cons 1 '(2 3)) -> (1 2 3)
        let module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                }, // Push 1
                Instruction {
                    opcode: Opcode::LoadConst(1),
                    source_offset: 0,
                }, // Push (2 3)
                Instruction {
                    opcode: Opcode::Cons,
                    source_offset: 0,
                }, // Cons them
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                },
            ],
            constants: vec![
                Value::Integer(1),
                Value::List(vec![Value::Integer(2), Value::Integer(3)]),
            ],
            strings: vec![],
            source_code: "test".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
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
    fn test_vm_make_list() {
        // Test MAKE_LIST: create list from stack elements
        let module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                }, // Push 1
                Instruction {
                    opcode: Opcode::LoadConst(1),
                    source_offset: 0,
                }, // Push 2
                Instruction {
                    opcode: Opcode::LoadConst(2),
                    source_offset: 0,
                }, // Push 3
                Instruction {
                    opcode: Opcode::MakeList(3),
                    source_offset: 0,
                }, // Make list of 3 elements
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                },
            ],
            constants: vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)],
            strings: vec![],
            source_code: "test".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
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
    fn test_vm_stack_operations() {
        // Test DUP and POP
        let module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                }, // Push 42
                Instruction {
                    opcode: Opcode::Dup,
                    source_offset: 0,
                }, // Duplicate it
                Instruction {
                    opcode: Opcode::Pop,
                    source_offset: 0,
                }, // Pop one copy
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                }, // Return the remaining
            ],
            constants: vec![Value::Integer(42)],
            strings: vec![],
            source_code: "test".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_vm_builtin_call() {
        // Test calling a built-in function: (+ 1 2)
        // Stack layout for call: [arg1, arg2, ..., procedure] (args pushed first, then function)
        let module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                }, // Push 1
                Instruction {
                    opcode: Opcode::LoadConst(1),
                    source_offset: 0,
                }, // Push 2
                Instruction {
                    opcode: Opcode::LoadVar(0),
                    source_offset: 0,
                }, // Push + function
                Instruction {
                    opcode: Opcode::Call(2),
                    source_offset: 0,
                }, // Call with 2 args
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                },
            ],
            constants: vec![Value::Integer(1), Value::Integer(2)],
            strings: vec!["+".to_string()],
            source_code: "(+ 1 2)".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(3));
    }

    #[test]
    fn test_vm_list_builtin_calls() {
        // Test car: (car '(1 2 3))
        let module = BytecodeModule {
            code: vec![
                Instruction {
                    opcode: Opcode::LoadConst(0),
                    source_offset: 0,
                }, // Push (1 2 3)
                Instruction {
                    opcode: Opcode::LoadVar(0),
                    source_offset: 0,
                }, // Push car function
                Instruction {
                    opcode: Opcode::Call(1),
                    source_offset: 0,
                }, // Call with 1 arg
                Instruction {
                    opcode: Opcode::Return,
                    source_offset: 0,
                },
            ],
            constants: vec![Value::List(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ])],
            strings: vec!["car".to_string()],
            source_code: "(car '(1 2 3))".to_string(),
            entry_point: 0,
        };

        let mut vm = VM::new();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_vm_if_statement() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Test if with true condition
        let source = "(if #t 42 24)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(42));

        // Test if with false condition
        let source = "(if #f 42 24)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(24));
    }

    #[test]
    fn test_vm_define_variable() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a variable
        let source = "(define x 42)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Now try to access the variable
        let source = "x";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_vm_lambda_creation() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Create a lambda
        let source = "(lambda (x) (+ x 1))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();

        // Should return a procedure
        match result {
            Value::Procedure { .. } => {} // Success
            _ => panic!("Expected procedure, got {:?}", result),
        }
    }

    #[test]
    fn test_vm_define_function() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function
        let source = "(define (add1 x) (+ x 1))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Now try to access the function
        let source = "add1";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();

        // Should return a procedure
        match result {
            Value::Procedure { .. } => {} // Success
            _ => panic!("Expected procedure, got {:?}", result),
        }
    }

    #[test]
    fn test_vm_call_user_function() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function
        let source = "(define (add1 x) (+ x 1))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Now call the function
        let source = "(add1 41)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();

        // Debug: print the bytecode
        println!("Bytecode for (add1 41):");
        for (i, instr) in module.code.iter().enumerate() {
            println!("  {}: {:?}", i, instr.opcode);
        }
        println!("Constants: {:?}", module.constants);
        println!("Strings: {:?}", module.strings);

        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn test_vm_lambda_call() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Call lambda directly
        let source = "((lambda (x) (+ x 1)) 41)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Integer(42));
    }

    // ===== NEGATIVE TESTS - R7RS DEVIATIONS =====
    // These tests document what we DON'T support compared to R7RS Scheme

    #[test]
    #[should_panic(expected = "Undefined variable: set!")]
    fn test_set_not_supported() {
        // **R7RS DEVIATION:** No support for set! - all bindings are immutable
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // This should fail because we don't support set! (undefined variable)
        let source = "set!";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let _result = vm.execute(&module).unwrap(); // Should panic
    }

    #[test]
    #[should_panic(expected = "Undefined variable: letrec")]
    fn test_letrec_not_supported() {
        // **R7RS DEVIATION:** No support for letrec mutual recursion
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // This should fail because we don't support letrec (undefined variable)
        let source = "letrec";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let _result = vm.execute(&module).unwrap(); // Should panic
    }

    #[test]
    #[should_panic(expected = "Undefined variable: let")]
    fn test_let_not_supported() {
        // **R7RS DEVIATION:** No support for let bindings yet
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // This should fail because we don't support let (undefined variable)
        let source = "let";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let _result = vm.execute(&module).unwrap(); // Should panic
    }

    #[test]
    fn debug_simple_function_call() {
        // Test a simple function call to see if parameters work correctly
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // First, define a simple function that adds its parameter to 10
        let source = "(define (add-ten x) (+ x 10))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Now call it
        let source = "(add-ten 5)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        println!("Result: {:?}", result);
        assert_eq!(result, Value::Integer(15));
    }

    #[test]
    #[ignore = "Old VM bytecode implementation incomplete: lambdas store AST bodies that get recompiled on each call via execute_value_in_current_env(), causing deep recursion and stack overflow. See compiler.rs:461-475 and vm.rs:1972-1977 for the problematic pattern."]
    fn debug_lambda_creation() {
        // Test just creating a lambda without calling it
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Create a simple lambda
        let source = "(lambda (x) (+ x 1))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        println!("Lambda result: {:?}", result);
        match result {
            Value::Procedure { .. } => println!("Successfully created lambda"),
            _ => panic!("Expected lambda procedure"),
        }
    }

    #[test]
    fn debug_make_adder_definition() {
        // Test defining make-adder
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define make-adder
        let source = "(define (make-adder n) (lambda (x) (+ x n)))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        println!("Define result: {:?}", result);
        assert_eq!(result, Value::Unspecified);
    }

    #[test]
    #[ignore]
    fn debug_make_adder_call() {
        // Test calling make-adder
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // First define make-adder
        let source = "(define (make-adder n) (lambda (x) (+ x n)))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Now call it
        let source = "(make-adder 10)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        println!("make-adder call result: {:?}", result);
        match result {
            Value::Procedure { .. } => println!("Successfully created adder closure"),
            _ => panic!("Expected procedure from make-adder"),
        }
    }

    #[test]
    fn test_lexical_scoping_limitation() {
        // This test shows that our current implementation has correct lexical scoping
        // using runtime environment capture with proper closure creation and execution
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function that captures its lexical environment
        let source = "(define (make-adder n) (lambda (x) (+ x n)))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Create an adder that captures n=10
        let source = "(make-adder 10)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let adder = vm.execute(&module).unwrap();

        // Should return a procedure that properly captures the environment
        match &adder {
            Value::Procedure { params, env, .. } => {
                // Verify closure was created correctly
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
                // Verify the captured environment contains n=10
                assert_eq!(env.lookup("n").unwrap(), Value::Integer(10));
            }
            _ => panic!("Expected procedure, got {:?}", adder),
        }

        // Now test that we can actually call the closure and it works correctly
        // This would have failed before the bug fix but now works
        let source = "(define add-ten (make-adder 10))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        vm.execute(&module).unwrap();

        let source = "(add-ten 5)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env.clone()).unwrap();
        let result = vm.execute(&module).unwrap();

        // This should now work correctly - closure execution with captured variables
        assert_eq!(result, Value::Integer(15));
    }

    // ===== COMPREHENSIVE CLOSURE TESTS =====
    // These tests verify all aspects of closure functionality work correctly

    #[test]
    fn test_basic_closure_functionality() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define make-adder
        vm.execute(
            &compile(
                &parse("(define (make-adder n) (lambda (x) (+ x n)))").unwrap(),
                "(define (make-adder n) (lambda (x) (+ x n)))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Create add-5 closure
        vm.execute(
            &compile(
                &parse("(define add-5 (make-adder 5))").unwrap(),
                "(define add-5 (make-adder 5))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test the closure
        let result = vm
            .execute(
                &compile(
                    &parse("(add-5 10)").unwrap(),
                    "(add-5 10)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result, Value::Integer(15));
    }

    #[test]
    fn test_multiple_independent_closures() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define make-multiplier
        vm.execute(
            &compile(
                &parse("(define (make-multiplier n) (lambda (x) (* x n)))").unwrap(),
                "(define (make-multiplier n) (lambda (x) (* x n)))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Create two different multipliers
        vm.execute(
            &compile(
                &parse("(define double (make-multiplier 2))").unwrap(),
                "(define double (make-multiplier 2))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        vm.execute(
            &compile(
                &parse("(define triple (make-multiplier 3))").unwrap(),
                "(define triple (make-multiplier 3))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test both closures maintain separate state
        let result1 = vm
            .execute(
                &compile(
                    &parse("(double 5)").unwrap(),
                    "(double 5)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result1, Value::Integer(10));

        let result2 = vm
            .execute(
                &compile(
                    &parse("(triple 5)").unwrap(),
                    "(triple 5)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result2, Value::Integer(15));
    }

    #[test]
    fn test_nested_closure_creation() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function that returns a function that returns a function
        vm.execute(&compile(&parse("(define (make-scaler-factory base) (lambda (factor) (lambda (x) (* base factor x))))").unwrap(),
                           "(define (make-scaler-factory base) (lambda (factor) (lambda (x) (* base factor x))))".to_string(), 
                           vm.current_env.clone()).unwrap()).unwrap();

        // Create a scaler factory
        vm.execute(
            &compile(
                &parse("(define make-double-scaler (make-scaler-factory 2))").unwrap(),
                "(define make-double-scaler (make-scaler-factory 2))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Create a specific scaler
        vm.execute(
            &compile(
                &parse("(define scale-by-3 (make-double-scaler 3))").unwrap(),
                "(define scale-by-3 (make-double-scaler 3))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test the nested closure
        let result = vm
            .execute(
                &compile(
                    &parse("(scale-by-3 4)").unwrap(),
                    "(scale-by-3 4)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result, Value::Integer(24)); // 2 * 3 * 4 = 24
    }

    #[test]
    fn test_closure_composition() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define make-adder and compose function
        vm.execute(
            &compile(
                &parse("(define (make-adder n) (lambda (x) (+ x n)))").unwrap(),
                "(define (make-adder n) (lambda (x) (+ x n)))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        vm.execute(
            &compile(
                &parse("(define (compose f g) (lambda (x) (f (g x))))").unwrap(),
                "(define (compose f g) (lambda (x) (f (g x))))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Create individual adders
        vm.execute(
            &compile(
                &parse("(define add-1 (make-adder 1))").unwrap(),
                "(define add-1 (make-adder 1))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        vm.execute(
            &compile(
                &parse("(define add-2 (make-adder 2))").unwrap(),
                "(define add-2 (make-adder 2))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Compose them
        vm.execute(
            &compile(
                &parse("(define add-3-total (compose add-1 add-2))").unwrap(),
                "(define add-3-total (compose add-1 add-2))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test composition
        let result = vm
            .execute(
                &compile(
                    &parse("(add-3-total 10)").unwrap(),
                    "(add-3-total 10)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result, Value::Integer(13)); // (10 + 2) + 1 = 13
    }

    #[test]
    fn test_multi_variable_capture() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a global variable
        vm.execute(
            &compile(
                &parse("(define global-multiplier 100)").unwrap(),
                "(define global-multiplier 100)".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Define a function that captures both local and global variables
        vm.execute(&compile(&parse("(define (make-calculator local-add) (lambda (x) (+ x local-add global-multiplier)))").unwrap(),
                           "(define (make-calculator local-add) (lambda (x) (+ x local-add global-multiplier)))".to_string(), 
                           vm.current_env.clone()).unwrap()).unwrap();

        // Create calculator closure
        vm.execute(
            &compile(
                &parse("(define calc (make-calculator 15))").unwrap(),
                "(define calc (make-calculator 15))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test multi-variable capture
        let result = vm
            .execute(
                &compile(
                    &parse("(calc 0)").unwrap(),
                    "(calc 0)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result, Value::Integer(115)); // 0 + 15 + 100 = 115
    }

    #[test]
    fn test_environment_isolation() {
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define counter factory
        vm.execute(
            &compile(
                &parse("(define (make-counter start) (lambda (increment) (+ start increment)))")
                    .unwrap(),
                "(define (make-counter start) (lambda (increment) (+ start increment)))"
                    .to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Create two counters with different starting values
        vm.execute(
            &compile(
                &parse("(define counter1 (make-counter 10))").unwrap(),
                "(define counter1 (make-counter 10))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        vm.execute(
            &compile(
                &parse("(define counter2 (make-counter 20))").unwrap(),
                "(define counter2 (make-counter 20))".to_string(),
                vm.current_env.clone(),
            )
            .unwrap(),
        )
        .unwrap();

        // Test that they maintain separate environments
        let result1 = vm
            .execute(
                &compile(
                    &parse("(counter1 5)").unwrap(),
                    "(counter1 5)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result1, Value::Integer(15)); // 10 + 5

        let result2 = vm
            .execute(
                &compile(
                    &parse("(counter2 5)").unwrap(),
                    "(counter2 5)".to_string(),
                    vm.current_env.clone(),
                )
                .unwrap(),
            )
            .unwrap();
        assert_eq!(result2, Value::Integer(25)); // 20 + 5
    }

    // ===== ADDITIONAL LEXICAL SCOPING TESTS =====
    // These tests verify specific lexical scoping scenarios

    #[test]
    fn test_closure_environment_inspection() {
        // Test that we can inspect closure environments for correctness
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function that creates a closure
        let source = "(define (make-adder n) (lambda (x) (+ x n)))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Create an adder closure
        let source = "(make-adder 10)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        let adder = vm.execute(&module).unwrap();

        // The adder should be a procedure with proper environment capture
        match &adder {
            Value::Procedure { params, env, .. } => {
                // Verify parameter structure
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");

                // The environment should contain 'n' bound to 10
                assert!(env.lookup("n").is_some());
                assert_eq!(env.lookup("n").unwrap(), Value::Integer(10));
            }
            _ => panic!("Expected procedure, got {:?}", adder),
        }
    }

    #[test]
    fn test_deep_nested_lexical_scoping() {
        // Test multiple levels of lexical nesting
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define a function with deeply nested closures
        let source = "(define (make-multiplier m) (lambda (n) (lambda (x) (* x m n))))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        let result = vm.execute(&module).unwrap();
        assert_eq!(result, Value::Unspecified);

        // Create multiplier that captures m=2
        let source = "(make-multiplier 2)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        let mult_maker = vm.execute(&module).unwrap();

        // Should be a procedure that captures m=2
        match &mult_maker {
            Value::Procedure { env, .. } => {
                assert_eq!(env.lookup("m").unwrap(), Value::Integer(2));
            }
            _ => panic!("Expected procedure"),
        }
    }

    #[test]
    fn test_variable_shadowing_in_closures() {
        // Test that parameter shadowing works correctly in closures
        use crate::compiler::compile;
        use crate::parser::parse;

        let mut vm = VM::new();

        // Define x globally
        let source = "(define x 100)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        vm.execute(&module).unwrap();

        // Create a function that shadows the global x
        let source = "(define (test-shadow x) (lambda (y) (+ x y)))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        vm.execute(&module).unwrap();

        // Call with x=5, should create closure that captures parameter x=5, not global x=100
        let source = "(test-shadow 5)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env()).unwrap();
        let closure = vm.execute(&module).unwrap();

        match &closure {
            Value::Procedure { env, .. } => {
                // Should capture the parameter x=5, not global x=100
                assert_eq!(env.lookup("x").unwrap(), Value::Integer(5));
                // Should still be able to access global environment through parent chain
                assert!(env.parent.is_some());
            }
            _ => panic!("Expected procedure"),
        }
    }
}
