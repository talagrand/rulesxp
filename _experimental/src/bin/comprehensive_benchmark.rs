use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    processed_ast::ProcessedAST,
    processed_env::ProcessedEnvironment,
    super_builtins::ProcessedValue,
    super_vm::{SuperDirectVM, SuperStackVM},
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== COMPREHENSIVE EVALUATION BENCHMARK ===");

    // Global iteration count for all benchmarks
    const ITERATIONS: usize = 1000;
    println!("Running {} iterations for all operations", ITERATIONS);
    println!();

    // Load benchmark program
    let code = std::fs::read_to_string("simple_benchmark.scm")?;
    println!("Loaded benchmark: {} characters", code.len());
    println!("Program: {}", code.trim());
    println!();

    // Create environment with builtins
    let env = {
        let global_env = Environment::new();
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
        Rc::new(global_env)
    };

    // Parse multiple iterations to get average
    println!("=== PARSING ===");
    let mut parse_times = Vec::new();
    let mut expressions = Vec::new();

    for i in 0..ITERATIONS {
        let parse_start = Instant::now();
        let parsed = parser::parse_multiple(&code)?;
        let parse_time = parse_start.elapsed();
        parse_times.push(parse_time);
        if i == 0 {
            expressions = parsed; // Save first parse result
        }
    }

    let parse_avg = parse_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let parse_min = parse_times.iter().min().unwrap();
    let parse_max = parse_times.iter().max().unwrap();
    println!(
        "Parse time - Avg: {:?}, Min: {:?}, Max: {:?}",
        parse_avg, parse_min, parse_max
    );
    println!("Expressions: {}", expressions.len());
    println!();

    // Macro expansion multiple iterations
    println!("=== MACRO EXPANSION ===");
    let mut macro_times = Vec::new();
    let mut expanded_exprs = Vec::new();

    for i in 0..ITERATIONS {
        let macro_start = Instant::now();
        let mut macro_expander = MacroExpander::new(env.clone());
        macro_expander.load_prelude()?;

        let mut current_expanded = Vec::new();
        for expr in &expressions {
            let expanded = macro_expander.expand(expr)?;
            current_expanded.push(expanded);
        }
        let macro_time = macro_start.elapsed();
        macro_times.push(macro_time);
        if i == 0 {
            expanded_exprs = current_expanded; // Save first expansion result
        }
    }

    let macro_avg = macro_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let macro_min = macro_times.iter().min().unwrap();
    let macro_max = macro_times.iter().max().unwrap();
    println!(
        "Macro expansion time - Avg: {:?}, Min: {:?}, Max: {:?}",
        macro_avg, macro_min, macro_max
    );
    println!();

    // Combine expressions for unified evaluation
    let combined_program = if expanded_exprs.len() == 1 {
        expanded_exprs[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expanded_exprs.clone());
        Value::List(begin_list)
    };

    println!(
        "Running {} iterations of each evaluation mode...",
        ITERATIONS
    );
    println!();

    // 1. Direct AST Interpretation Benchmark
    println!("=== 1. DIRECT AST INTERPRETATION ===");
    let mut ast_times = Vec::new();
    let mut ast_result = Value::Integer(0);

    for i in 0..ITERATIONS {
        let start = Instant::now();
        let mut vm = VM::new_direct_interpreter(env.clone());
        ast_result = vm.evaluate_ast(&combined_program)?;
        let elapsed = start.elapsed();
        ast_times.push(elapsed);
        if i == 0 {
            println!("First result: {:?}", ast_result);
        }
    }

    let ast_avg = ast_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let ast_min = ast_times.iter().min().unwrap();
    let ast_max = ast_times.iter().max().unwrap();
    println!(
        "Direct AST - Avg: {:?}, Min: {:?}, Max: {:?}",
        ast_avg, ast_min, ast_max
    );
    println!();

    // 2. Stack-based AST Interpretation Benchmark
    println!("=== 2. STACK-BASED AST INTERPRETATION ===");
    let mut stack_ast_times = Vec::new();
    let mut stack_ast_result = Value::Integer(0);

    for i in 0..ITERATIONS {
        let start = Instant::now();
        let mut vm = VM::new_stack_ast_interpreter(env.clone());
        stack_ast_result = vm.evaluate_ast_stack(&combined_program)?;
        let elapsed = start.elapsed();
        stack_ast_times.push(elapsed);
        if i == 0 {
            println!("First result: {:?}", stack_ast_result);
        }
    }

    let stack_ast_avg = stack_ast_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let stack_ast_min = stack_ast_times.iter().min().unwrap();
    let stack_ast_max = stack_ast_times.iter().max().unwrap();
    println!(
        "Stack AST - Avg: {:?}, Min: {:?}, Max: {:?}",
        stack_ast_avg, stack_ast_min, stack_ast_max
    );
    println!();

    // 3. Non-CPS Bytecode Benchmark
    println!("=== 3. NON-CPS BYTECODE EVALUATION ===");

    // Compile multiple iterations to get average compilation time
    let mut compile_times = Vec::new();
    let mut module = None;

    for i in 0..ITERATIONS {
        let compile_start = Instant::now();
        let compiled = compiler::compile(&combined_program, code.clone(), env.clone())?;
        let compile_time = compile_start.elapsed();
        compile_times.push(compile_time);
        if i == 0 {
            module = Some(compiled); // Save first compilation result
        }
    }

    let compile_avg = compile_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let compile_min = compile_times.iter().min().unwrap();
    let compile_max = compile_times.iter().max().unwrap();
    println!(
        "Bytecode compilation time - Avg: {:?}, Min: {:?}, Max: {:?}",
        compile_avg, compile_min, compile_max
    );

    let module = module.unwrap();
    let mut bytecode_times = Vec::new();
    let mut bytecode_result = Value::Integer(0);

    for i in 0..ITERATIONS {
        let start = Instant::now();
        let mut vm = VM::new_with_env(env.clone(), false);
        bytecode_result = vm.execute(&module)?;
        let elapsed = start.elapsed();
        bytecode_times.push(elapsed);
        if i == 0 {
            println!("First result: {:?}", bytecode_result);
        }
    }

    let bytecode_avg = bytecode_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let bytecode_min = bytecode_times.iter().min().unwrap();
    let bytecode_max = bytecode_times.iter().max().unwrap();
    println!(
        "Bytecode execution - Avg: {:?}, Min: {:?}, Max: {:?}",
        bytecode_avg, bytecode_min, bytecode_max
    );
    println!(
        "Total bytecode (compile + avg execute): {:?}",
        compile_avg + bytecode_avg
    );
    println!();

    // 4. ProcessedAST Compilation + Direct Evaluation Benchmark
    println!("=== 4. PROCESSED AST DIRECT EVALUATION ===");

    let mut super_direct_compile_times = Vec::new();
    let mut super_direct_exec_times = Vec::new();
    let mut super_direct_result = ProcessedValue::Integer(0);

    for i in 0..ITERATIONS {
        // Time compilation separately
        let compile_start = Instant::now();
        let compiled_ast = ProcessedAST::compile(&combined_program)?;
        let compile_time = compile_start.elapsed();
        super_direct_compile_times.push(compile_time);

        // Time execution separately
        let exec_start = Instant::now();
        let mut vm = SuperDirectVM::new(compiled_ast);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(Rc::new(env))?;
        super_direct_result = result;
        let exec_time = exec_start.elapsed();
        super_direct_exec_times.push(exec_time);

        if i == 0 {
            println!("First result: {:?}", super_direct_result);
        }
    }

    let processed_compile_avg = super_direct_compile_times
        .iter()
        .sum::<std::time::Duration>()
        / ITERATIONS as u32;
    let processed_compile_min = super_direct_compile_times.iter().min().unwrap();
    let processed_compile_max = super_direct_compile_times.iter().max().unwrap();
    println!(
        "ProcessedAST compilation time - Avg: {:?}, Min: {:?}, Max: {:?}",
        processed_compile_avg, processed_compile_min, processed_compile_max
    );

    let super_direct_avg =
        super_direct_exec_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let super_direct_min = super_direct_exec_times.iter().min().unwrap();
    let super_direct_max = super_direct_exec_times.iter().max().unwrap();
    println!(
        "ProcessedAST Direct execution (exec only) - Avg: {:?}, Min: {:?}, Max: {:?}",
        super_direct_avg, super_direct_min, super_direct_max
    );
    println!(
        "Total ProcessedAST Direct (compile + avg execute): {:?}",
        processed_compile_avg + super_direct_avg
    );
    println!();

    // 5. ProcessedAST Compilation + Stack Evaluation Benchmark
    println!("=== 5. PROCESSED AST STACK EVALUATION ===");

    let mut super_stack_exec_times = Vec::new();
    let mut super_stack_result = ProcessedValue::Integer(0);

    for i in 0..ITERATIONS {
        // Reuse compilation timing from Direct mode (same compilation process)
        let compile_start = Instant::now();
        let compiled_ast = ProcessedAST::compile(&combined_program)?;
        let _compile_time = compile_start.elapsed(); // Not tracking separately for stack mode

        // Time execution separately
        let exec_start = Instant::now();
        let mut vm = SuperStackVM::new(compiled_ast);
        let env = ProcessedEnvironment::new();
        let result = vm.evaluate(Rc::new(env))?;
        super_stack_result = result;
        let exec_time = exec_start.elapsed();
        super_stack_exec_times.push(exec_time);

        if i == 0 {
            println!("First result: {:?}", super_stack_result);
        }
    }

    let super_stack_avg =
        super_stack_exec_times.iter().sum::<std::time::Duration>() / ITERATIONS as u32;
    let super_stack_min = super_stack_exec_times.iter().min().unwrap();
    let super_stack_max = super_stack_exec_times.iter().max().unwrap();
    println!(
        "ProcessedAST Stack execution (exec only) - Avg: {:?}, Min: {:?}, Max: {:?}",
        super_stack_avg, super_stack_min, super_stack_max
    );
    println!(
        "Total ProcessedAST Stack (compile + avg execute): {:?}",
        processed_compile_avg + super_stack_avg
    );
    println!();

    // Results verification
    println!("=== RESULTS VERIFICATION ===");
    println!("Direct AST result: {:?}", ast_result);
    println!("Stack AST result: {:?}", stack_ast_result);
    println!("Bytecode result: {:?}", bytecode_result);
    println!("ProcessedAST Direct result: {:?}", super_direct_result);
    println!("ProcessedAST Stack result: {:?}", super_stack_result);
    println!();

    // Performance summary
    println!("=== PERFORMANCE SUMMARY ===");
    println!("1. Direct AST:           {:?} (baseline)", ast_avg);
    println!(
        "2. Stack AST:            {:?} ({:.2}x vs Direct AST)",
        stack_ast_avg,
        stack_ast_avg.as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "3. Bytecode (exec only): {:?} ({:.2}x vs Direct AST)",
        bytecode_avg,
        bytecode_avg.as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "4. Bytecode (total):     {:?} ({:.2}x vs Direct AST)",
        compile_avg + bytecode_avg,
        (compile_avg + bytecode_avg).as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "5. SuperVM Direct (exec): {:?} ({:.2}x vs Direct AST)",
        super_direct_avg,
        super_direct_avg.as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "6. SuperVM Direct (total): {:?} ({:.2}x vs Direct AST)",
        processed_compile_avg + super_direct_avg,
        (processed_compile_avg + super_direct_avg).as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "7. SuperVM Stack (exec):  {:?} ({:.2}x vs Direct AST)",
        super_stack_avg,
        super_stack_avg.as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!(
        "8. SuperVM Stack (total): {:?} ({:.2}x vs Direct AST)",
        processed_compile_avg + super_stack_avg,
        (processed_compile_avg + super_stack_avg).as_nanos() as f64 / ast_avg.as_nanos() as f64
    );
    println!();

    // Compilation time comparison
    println!("=== COMPILATION TIME COMPARISON ===");
    println!("Bytecode compilation:    {:?}", compile_avg);
    println!(
        "ProcessedAST compilation: {:?} ({:.2}x vs Bytecode)",
        processed_compile_avg,
        processed_compile_avg.as_nanos() as f64 / compile_avg.as_nanos() as f64
    );

    Ok(())
}
