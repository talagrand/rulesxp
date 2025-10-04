use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;
use std::time::{Duration, Instant};

fn main() {
    println!("CPS Builtin Performance Benchmark");
    println!("==================================");

    // Simple arithmetic expressions for benchmarking
    let test_expressions = vec![
        "(+ 1 2)",
        "(* 3 4)",
        "(- 10 5)",
        "(+ (* 2 3) (- 8 3))", // Nested arithmetic
        "(= 5 5)",
        "(< 3 7)",
    ];

    const ITERATIONS: usize = 1000;

    for expr in &test_expressions {
        println!("\nBenchmarking: {}", expr);
        benchmark_expression(expr, ITERATIONS);
    }
}

fn benchmark_expression(code: &str, iterations: usize) {
    // Parse once
    let ast = parser::parse(code).expect("Parse failed");

    // Create environment with CPS builtins
    let env = {
        let global_env = Environment::new();
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
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

    // Benchmark CPS compilation (including macro expansion)
    let mut compilation_times = Vec::new();
    for _ in 0..iterations {
        let start = Instant::now();

        // Macro expansion (part of compilation)
        let mut cps_macro_expander = MacroExpander::new(env.clone());
        cps_macro_expander
            .load_cps_prelude()
            .expect("Failed to load CPS prelude");
        let expanded = cps_macro_expander
            .expand(&ast)
            .expect("CPS macro expansion failed");

        // Compilation
        let _bytecode = compiler::compile_with_cps(&expanded, code.to_string(), env.clone())
            .expect("CPS compilation failed");
        compilation_times.push(start.elapsed());
    }

    // Pre-compile for evaluation benchmark
    let mut cps_macro_expander = MacroExpander::new(env.clone());
    cps_macro_expander
        .load_cps_prelude()
        .expect("Failed to load CPS prelude");
    let expanded = cps_macro_expander
        .expand(&ast)
        .expect("CPS macro expansion failed");
    let bytecode = compiler::compile_with_cps(&expanded, code.to_string(), env.clone())
        .expect("CPS compilation failed");

    // Benchmark CPS evaluation
    let mut evaluation_times = Vec::new();
    for _ in 0..iterations {
        let start = Instant::now();
        let mut vm = VM::new_with_cps(true);
        let _result = vm.execute(&bytecode).expect("CPS execution failed");
        evaluation_times.push(start.elapsed());
    }

    // Calculate statistics
    let avg_compilation = compilation_times.iter().sum::<Duration>() / iterations as u32;
    let avg_evaluation = evaluation_times.iter().sum::<Duration>() / iterations as u32;
    let total_compilation = compilation_times.iter().sum::<Duration>();
    let total_evaluation = evaluation_times.iter().sum::<Duration>();

    println!("  Compilation ({} iterations):", iterations);
    println!("    Total time: {:?}", total_compilation);
    println!("    Average per iteration: {:?}", avg_compilation);
    println!(
        "    Iterations per second: {:.0}",
        1.0 / avg_compilation.as_secs_f64()
    );

    println!("  Evaluation ({} iterations):", iterations);
    println!("    Total time: {:?}", total_evaluation);
    println!("    Average per iteration: {:?}", avg_evaluation);
    println!(
        "    Iterations per second: {:.0}",
        1.0 / avg_evaluation.as_secs_f64()
    );
}
