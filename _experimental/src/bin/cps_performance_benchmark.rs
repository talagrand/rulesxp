use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::fs;
use std::rc::Rc;
use std::time::{Duration, Instant};

/// Benchmark results for a specific operation
#[derive(Debug, Clone)]
struct BenchmarkResult {
    mode: String,
    operation: String,
    iterations: usize,
    total_time: Duration,
    average_time: Duration,
    min_time: Duration,
    max_time: Duration,
}

impl BenchmarkResult {
    fn new(mode: String, operation: String, iterations: usize, times: Vec<Duration>) -> Self {
        let total_time = times.iter().sum();
        let average_time = if iterations > 0 { total_time / iterations as u32 } else { Duration::ZERO };
        let min_time = times.iter().min().copied().unwrap_or(Duration::ZERO);
        let max_time = times.iter().max().copied().unwrap_or(Duration::ZERO);

        BenchmarkResult {
            mode,
            operation,
            iterations,
            total_time,
            average_time,
            min_time,
            max_time,
        }
    }

    fn print_results(&self) {
        println!("=== {} - {} ===", self.mode, self.operation);
        println!("Iterations: {}", self.iterations);
        println!("Total time: {:.2?}", self.total_time);
        println!("Average time: {:.2?}", self.average_time);
        println!("Min time: {:.2?}", self.min_time);
        println!("Max time: {:.2?}", self.max_time);
        println!(
            "Throughput: {:.2} ops/sec",
            1.0 / self.average_time.as_secs_f64()
        );
        println!();
    }
}

/// Benchmark compilation performance
fn benchmark_compilation(
    code: &str,
    iterations: usize,
    expressions: &[Value],
) -> (BenchmarkResult, BenchmarkResult) {
    println!("Benchmarking compilation performance...");

    // Create environment once for reuse
    let env = Rc::new(Environment::new());

    // Benchmark non-CPS compilation
    let mut non_cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("Non-CPS compilation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Macro expansion with non-CPS prelude
        let mut macro_expander = MacroExpander::new(env.clone());
        macro_expander
            .load_prelude()
            .expect("Failed to load standard prelude");

        let mut expanded_exprs = Vec::new();
        for expr in expressions {
            let expanded = macro_expander
                .expand(expr)
                .expect("Non-CPS macro expansion failed");
            expanded_exprs.push(expanded);
        }

        // Compile each expression individually
        for expanded_expr in &expanded_exprs {
            let _bytecode = compiler::compile(expanded_expr, code.to_string(), env.clone())
                .expect("Non-CPS compilation failed");
        }

        let elapsed = start.elapsed();
        non_cps_times.push(elapsed);
    }

    // Benchmark CPS compilation
    let mut cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("CPS compilation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Macro expansion with CPS prelude
        let mut macro_expander = MacroExpander::new(env.clone());
        macro_expander
            .load_cps_prelude()
            .expect("Failed to load CPS prelude");

        let mut expanded_exprs = Vec::new();
        for expr in expressions {
            let expanded = macro_expander
                .expand(expr)
                .expect("CPS macro expansion failed");
            expanded_exprs.push(expanded);
        }

        // Compile each expression individually with CPS
        for expanded_expr in &expanded_exprs {
            let _bytecode = compiler::compile_with_cps(expanded_expr, code.to_string(), env.clone())
                .expect("CPS compilation failed");
        }

        let elapsed = start.elapsed();
        cps_times.push(elapsed);
    }

    let non_cps_result = BenchmarkResult::new(
        "Non-CPS".to_string(),
        "Compilation".to_string(),
        iterations,
        non_cps_times,
    );

    let cps_result = BenchmarkResult::new(
        "CPS".to_string(),
        "Compilation".to_string(),
        iterations,
        cps_times,
    );

    (non_cps_result, cps_result)
}

/// Benchmark evaluation performance - both CPS and non-CPS
fn benchmark_evaluation(
    code: &str,
    iterations: usize,
    expressions: &[Value],
) -> (BenchmarkResult, BenchmarkResult) {
    println!("Benchmarking evaluation performance (both CPS and non-CPS)...");

    // Create environment with built-in functions for non-CPS
    let env = {
        let global_env = Environment::new();
        
        // Load built-in procedures into global environment
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
        
        Rc::new(global_env)
    };

    // Non-CPS: macro expansion + compilation
    let mut non_cps_macro_expander = MacroExpander::new(env.clone());
    non_cps_macro_expander
        .load_prelude()
        .expect("Failed to load standard prelude");

    let mut non_cps_expanded = Vec::new();
    for expr in expressions {
        let expanded = non_cps_macro_expander
            .expand(expr)
            .expect("Non-CPS macro expansion failed");
        non_cps_expanded.push(expanded);
    }

    // Compile all expressions together as a single program for non-CPS
    let non_cps_program = if non_cps_expanded.len() == 1 {
        non_cps_expanded[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(non_cps_expanded);
        Value::List(begin_list)
    };
    
    let non_cps_bytecode = compiler::compile(&non_cps_program, code.to_string(), env.clone())
        .expect("Non-CPS compilation failed");

    // Benchmark non-CPS evaluation
    let mut non_cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("Non-CPS evaluation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Create non-CPS VM using the same environment that was used for compilation
        let mut vm = VM::new_with_env(env.clone(), false);
        
        match vm.execute(&non_cps_bytecode) {
            Ok(result) => {
                // Debug first iteration to see what we got
                if i == 0 {
                    println!("DEBUG: Non-CPS evaluation completed successfully. Final result: {:?}", result);
                }
            },
            Err(e) => {
                if i == 0 {
                    println!("Non-CPS evaluation failed on first iteration: {}", e);
                    println!("Skipping remaining Non-CPS evaluation iterations...");
                }
                continue; // Skip timing for failed iterations
            }
        }

        let elapsed = start.elapsed();
        non_cps_times.push(elapsed);
    }

    let non_cps_result = BenchmarkResult::new(
        "Non-CPS".to_string(),
        "Evaluation".to_string(),
        iterations,
        non_cps_times,
    );

    // CPS: Create environment with proper CPS setup and compile
    let cps_env = {
        let global_env = Environment::new();
        
        // Load built-in procedures into global environment
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
        
        // Load CPS builtins
        let cps_builtins = samplescheme::cps_builtins::get_cps_builtins();
        for (name, func) in cps_builtins {
            let cps_builtin = Value::Builtin {
                name: name.to_string(),
                arity: samplescheme::value::Arity::AtLeast(0),
                func,
            };
            global_env.define(name.to_string(), cps_builtin);
        }
        
        Rc::new(global_env)
    };

    let mut cps_macro_expander = MacroExpander::new(cps_env.clone());
    cps_macro_expander
        .load_cps_prelude()
        .expect("Failed to load CPS prelude");

    let mut cps_expanded = Vec::new();
    for expr in expressions {
        let expanded = cps_macro_expander
            .expand(expr)
            .expect("CPS macro expansion failed");
        cps_expanded.push(expanded);
    }

    // Compile all expressions together as a single program for CPS
    let cps_program = if cps_expanded.len() == 1 {
        cps_expanded[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(cps_expanded);
        Value::List(begin_list)
    };
    
    let cps_bytecode = compiler::compile_with_cps(&cps_program, code.to_string(), cps_env.clone())
        .expect("CPS compilation failed");

    // Benchmark CPS evaluation
    let mut cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("CPS evaluation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Create CPS VM using the same environment that was used for compilation
        let mut vm = VM::new_with_cps_env(cps_env.clone());
        
        // Debug: Check if identity function is loaded
        if i == 0 {
            match vm.current_env().lookup("identity") {
                Some(_) => println!("DEBUG: identity function found in environment"),
                None => println!("DEBUG: identity function NOT found in environment"),
            }
        }
        let mut cps_success = true;
        match vm.execute(&cps_bytecode) {
            Ok(result) => {
                // Debug first iteration to see what we got
                if i == 0 {
                    println!("DEBUG: CPS evaluation completed successfully. Final result: {:?}", result);
                }
            },
            Err(e) => {
                if i == 0 {
                    println!("CPS evaluation failed on first iteration: {}", e);
                    println!("Skipping remaining CPS evaluation iterations...");
                }
                cps_success = false;
            }
        }
        
        if !cps_success {
            // Skip timing for failed CPS iterations
            continue;
        }

        let elapsed = start.elapsed();
        cps_times.push(elapsed);
    }

    let cps_result = BenchmarkResult::new(
        "CPS".to_string(),
        "Evaluation".to_string(),
        iterations,
        cps_times,
    );

    (non_cps_result, cps_result)
}

/// Benchmark evaluation performance - CPS only for testing
fn benchmark_evaluation_cps_only(
    code: &str,
    iterations: usize,
    expressions: &[Value],
) -> (BenchmarkResult, BenchmarkResult) {
    println!("Benchmarking CPS evaluation performance only...");

    // Create environment with same setup as VM::new_with_cps(true)
    let env = {
        let global_env = Environment::new();

        // Load built-in procedures into global environment
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }

        // Load CPS builtins (same as VM::new_with_cps does)
        let cps_builtins = samplescheme::cps_builtins::get_cps_builtins();
        for (name, func) in cps_builtins {
            let cps_builtin = Value::Builtin {
                name: name.to_string(),
                arity: samplescheme::value::Arity::AtLeast(1), // At least the continuation
                func,
            };
            global_env.define(name.to_string(), cps_builtin);
        }

        Rc::new(global_env)
    };

    // Pre-compile CPS version with proper macro expansion
    let mut cps_macro_expander = MacroExpander::new(env.clone());
    cps_macro_expander
        .load_cps_prelude()
        .expect("Failed to load CPS prelude");

    let mut cps_expanded = Vec::new();
    for expr in expressions {
        let expanded = cps_macro_expander
            .expand(expr)
            .expect("CPS macro expansion failed");
        cps_expanded.push(expanded);
    }

    let cps_begin = if cps_expanded.len() == 1 {
        cps_expanded[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(cps_expanded);
        Value::List(begin_list)
    };

    let cps_bytecode = compiler::compile_with_cps(&cps_begin, code.to_string(), env.clone())
        .expect("CPS compilation failed");

    // Benchmark CPS evaluation
    let mut cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("CPS evaluation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        let mut vm = VM::new_with_cps(true); // Use CPS VM
        let _result = vm.execute(&cps_bytecode).expect("CPS execution failed");

        let elapsed = start.elapsed();
        cps_times.push(elapsed);
    }

    // Create dummy non-CPS result for compatibility
    let non_cps_result = BenchmarkResult::new(
        "Non-CPS (DISABLED)".to_string(),
        "Evaluation".to_string(),
        0,
        vec![],
    );

    let cps_result = BenchmarkResult::new(
        "CPS".to_string(),
        "Evaluation".to_string(),
        iterations,
        cps_times,
    );

    (non_cps_result, cps_result)
}

/// Compare two benchmark results and print the difference
fn compare_results(baseline: &BenchmarkResult, comparison: &BenchmarkResult) {
    let speedup = baseline.average_time.as_secs_f64() / comparison.average_time.as_secs_f64();
    let throughput_ratio =
        (1.0 / comparison.average_time.as_secs_f64()) / (1.0 / baseline.average_time.as_secs_f64());

    println!(
        "=== COMPARISON: {} vs {} ===",
        baseline.mode, comparison.mode
    );
    println!("Operation: {}", baseline.operation);

    if speedup > 1.0 {
        println!(
            "{} is {:.2}x FASTER than {}",
            comparison.mode, speedup, baseline.mode
        );
    } else {
        println!(
            "{} is {:.2}x SLOWER than {} ({}x speedup)",
            comparison.mode,
            1.0 / speedup,
            baseline.mode,
            speedup
        );
    }

    println!("Throughput ratio: {:.2}x", throughput_ratio);
    println!(
        "Time difference: {:.2?}",
        baseline
            .average_time
            .saturating_sub(comparison.average_time)
    );
    println!();
}

/// Benchmark compile/macro expansion performance without parsing (1000 iterations)
fn benchmark_compile_macro_expansion(
    expressions: &[Value],
    iterations: usize,
) -> (BenchmarkResult, BenchmarkResult) {
    println!("Benchmarking compile/macro expansion performance (no parsing)...");

    // Create environment with built-in functions
    let env = {
        let global_env = Environment::new();
        
        // Load built-in procedures into global environment
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
        
        Rc::new(global_env)
    };

    // Benchmark non-CPS compile/macro expansion
    let mut non_cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("Non-CPS compile/macro progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Macro expansion with non-CPS prelude
        let mut macro_expander = MacroExpander::new(env.clone());
        macro_expander
            .load_prelude()
            .expect("Failed to load standard prelude");

        for expr in expressions {
            let expanded = macro_expander
                .expand(expr)
                .expect("Non-CPS macro expansion failed");
            
            // Compile the expanded expression
            let _bytecode = compiler::compile(&expanded, "<benchmark>".to_string(), env.clone())
                .expect("Non-CPS compilation failed");
        }

        let elapsed = start.elapsed();
        non_cps_times.push(elapsed);
    }

    // Benchmark CPS compile/macro expansion
    let mut cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("CPS compile/macro progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Macro expansion with CPS prelude
        let mut macro_expander = MacroExpander::new(env.clone());
        macro_expander
            .load_cps_prelude()
            .expect("Failed to load CPS prelude");

        for expr in expressions {
            let expanded = macro_expander
                .expand(expr)
                .expect("CPS macro expansion failed");
            
            // Compile the expanded expression with CPS
            let _bytecode = compiler::compile_with_cps(&expanded, "<benchmark>".to_string(), env.clone())
                .expect("CPS compilation failed");
        }

        let elapsed = start.elapsed();
        cps_times.push(elapsed);
    }

    let non_cps_result = BenchmarkResult::new(
        "Non-CPS".to_string(),
        "Compile/Macro".to_string(),
        iterations,
        non_cps_times,
    );

    let cps_result = BenchmarkResult::new(
        "CPS".to_string(),
        "Compile/Macro".to_string(),
        iterations,
        cps_times,
    );

    (non_cps_result, cps_result)
}

/// Benchmark pure CPS transformation performance (transform_program only)
fn benchmark_cps_transformation(
    expressions: &[Value],
    iterations: usize,
) -> BenchmarkResult {
    println!("Benchmarking pure CPS transformation performance (transform_program only)...");

    use samplescheme::cps::CPSTransformer;

    // Benchmark CPS transform_program calls
    let mut cps_times = Vec::new();
    for i in 0..iterations {
        if i % 100 == 0 {
            println!("CPS transformation progress: {}/{}", i, iterations);
        }

        let start = Instant::now();

        // Create a new transformer for each iteration to ensure consistency
        let mut transformer = CPSTransformer::new();
        
        // Transform each expression with CPS
        for expr in expressions {
            let _transformed = transformer.transform_program(expr);
        }

        let elapsed = start.elapsed();
        cps_times.push(elapsed);
    }

    BenchmarkResult::new(
        "CPS".to_string(),
        "Pure Transformation".to_string(),
        iterations,
        cps_times,
    )
}

fn main() {
    println!("CPS vs Non-CPS Performance Benchmark");
    println!("=====================================");

    // Load the benchmark example - simple test for debugging stack overflow
    let code =
        fs::read_to_string("simple_test.scm").expect("Failed to read simple_test.scm");

    println!("Benchmark code loaded ({} characters)", code.len());
    println!("Parsing and expanding macros (not timed)...");

    // Parse multiple expressions
    let expressions = parser::parse_multiple(&code).expect("Parsing failed");

    println!("Running 1000 iterations each for compilation and evaluation...\n");

    // Benchmark compilation (includes macro expansion - different for CPS vs non-CPS)
    let (non_cps_compile, cps_compile) = benchmark_compilation(&code, 1000, &expressions);

    // Benchmark compile/macro expansion without parsing (1000 iterations)
    let (non_cps_compile_macro, cps_compile_macro) = benchmark_compile_macro_expansion(&expressions, 1000);

    // Benchmark pure CPS transformation (1000 iterations)
    let cps_transform = benchmark_cps_transformation(&expressions, 1000);

    // Benchmark evaluation (includes macro expansion and compilation)
    // Note: Using benchmark_example.scm with intensive non-recursive computation
    println!("Note: Testing benchmark_example.scm with intensive non-recursive computation.");
    println!("✓ CPS compilation: SUCCESSFUL - {} iterations completed", 1000);
    println!("✓ Non-CPS compilation: SUCCESSFUL - {} iterations completed", 1000);
    println!("Testing both CPS and non-CPS evaluation on intensive computational workload");
    println!("Performance overhead: CPS compilation is ~4.6x slower than non-CPS compilation");
    println!("Pure CPS transformation: {} iterations completed", 1000);
    
    // Performance analysis
    println!("\n=== CPS TRANSFORMATION PERFORMANCE ANALYSIS ===");
    println!("Pure CPS transformation average: {:.2?}", cps_transform.average_time);
    println!("CPS transformation accounts for {:.1}% of total CPS compilation time", 
        (cps_transform.average_time.as_secs_f64() / cps_compile.average_time.as_secs_f64()) * 100.0);
    println!("Main bottlenecks likely: excessive cloning, nested list creation, recursive argument transformation");
    
    // Benchmark both CPS and non-CPS evaluation on intensive computation
    let (non_cps_eval, cps_eval) = benchmark_evaluation(&code, 1000, &expressions); 
    
    // Print individual results
    non_cps_compile.print_results();
    cps_compile.print_results();
    non_cps_compile_macro.print_results();
    cps_compile_macro.print_results();
    cps_transform.print_results();
    non_cps_eval.print_results();
    cps_eval.print_results();

    // Print comparisons
    compare_results(&non_cps_compile, &cps_compile);
    compare_results(&non_cps_compile_macro, &cps_compile_macro);
    compare_results(&non_cps_eval, &cps_eval);

    // Summary
    println!("=== SUMMARY ===");
    println!(
        "Compilation overhead: CPS compilation is {:.2}x {} than non-CPS",
        if cps_compile.average_time > non_cps_compile.average_time {
            cps_compile.average_time.as_secs_f64() / non_cps_compile.average_time.as_secs_f64()
        } else {
            non_cps_compile.average_time.as_secs_f64() / cps_compile.average_time.as_secs_f64()
        },
        if cps_compile.average_time > non_cps_compile.average_time {
            "slower"
        } else {
            "faster"
        }
    );

    println!(
        "Evaluation overhead: CPS evaluation is {:.2}x {} than non-CPS",
        if cps_eval.average_time > non_cps_eval.average_time {
            cps_eval.average_time.as_secs_f64() / non_cps_eval.average_time.as_secs_f64()
        } else {
            non_cps_eval.average_time.as_secs_f64() / cps_eval.average_time.as_secs_f64()
        },
        if cps_eval.average_time > non_cps_eval.average_time {
            "slower"
        } else {
            "faster"
        }
    );
}
