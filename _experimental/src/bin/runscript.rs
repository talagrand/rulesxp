use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    processed_ast::ProcessedAST,
    processed_env::ProcessedEnvironment,
    super_vm::{SuperDirectVM, SuperStackVM},
    value::{Environment, Value},
    vm::VM,
};
use std::env;
use std::fs;
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} <mode> <file.scm>", args[0]);
        eprintln!("Modes: ast, stackast, noncps, superast, superstackast");
        std::process::exit(1);
    }

    let mode = &args[1];
    let filename = &args[2];
    let source = fs::read_to_string(filename)?;

    println!("=== RUNSCRIPT ===");
    println!("Mode: {}", mode);
    println!("File: {}", filename);
    println!("Source: {}", source.trim());
    println!();

    // Parse the source
    let expressions = parser::parse_multiple(&source)?;
    println!("=== PARSED AST ===");
    for (i, expr) in expressions.iter().enumerate() {
        println!("Expression {}: {:?}", i, expr);
    }
    println!();

    // Create environment with builtins
    let env = {
        let global_env = Environment::new();

        // Load built-in procedures into global environment
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }

        Rc::new(global_env)
    };

    match mode.as_str() {
        "ast" => {
            println!("=== DIRECT AST INTERPRETATION ===");

            // Expand macros for AST interpretation
            let mut macro_expander = MacroExpander::new(env.clone());
            macro_expander.load_prelude()?;

            let mut expanded_exprs = Vec::new();
            for expr in &expressions {
                let expanded = macro_expander.expand(expr)?;
                expanded_exprs.push(expanded);
            }

            println!("=== MACRO-EXPANDED AST ===");
            for (i, expr) in expanded_exprs.iter().enumerate() {
                println!("Expanded {}: {:?}", i, expr);
            }
            println!();

            // Evaluate with direct AST interpreter
            let mut vm = VM::new_direct_interpreter(env.clone());
            let mut result = Value::Integer(0);

            for expanded_expr in &expanded_exprs {
                result = vm.evaluate_ast(expanded_expr)?;
                println!("AST evaluation result: {:?}", result);
            }

            println!("=== FINAL RESULT ===");
            println!("Direct AST: {:?}", result);
        }

        "stackast" => {
            println!("=== STACK-BASED AST INTERPRETATION ===");

            // Expand macros for AST interpretation
            let mut macro_expander = MacroExpander::new(env.clone());
            macro_expander.load_prelude()?;

            let mut expanded_exprs = Vec::new();
            for expr in &expressions {
                let expanded = macro_expander.expand(expr)?;
                expanded_exprs.push(expanded);
            }

            println!("=== MACRO-EXPANDED AST ===");
            for (i, expr) in expanded_exprs.iter().enumerate() {
                println!("Expanded {}: {:?}", i, expr);
            }
            println!();

            // Evaluate with stack-based AST interpreter
            let mut vm = VM::new_stack_ast_interpreter(env.clone());
            let mut result = Value::Integer(0);

            for expanded_expr in &expanded_exprs {
                result = vm.evaluate_ast_stack(expanded_expr)?;
                println!("Stack AST evaluation result: {:?}", result);
            }

            println!("=== FINAL RESULT ===");
            println!("Stack-based AST: {:?}", result);
        }

        "noncps" => {
            println!("=== NON-CPS BYTECODE COMPILATION ===");

            // Expand macros with standard prelude
            let mut macro_expander = MacroExpander::new(env.clone());
            macro_expander.load_prelude()?;

            let mut expanded_exprs = Vec::new();
            for expr in &expressions {
                let expanded = macro_expander.expand(expr)?;
                expanded_exprs.push(expanded);
            }

            println!("=== MACRO-EXPANDED AST ===");
            for (i, expr) in expanded_exprs.iter().enumerate() {
                println!("Expanded {}: {:?}", i, expr);
            }
            println!();

            // Combine all expressions into a begin block for compilation
            let non_cps_program = if expanded_exprs.len() == 1 {
                expanded_exprs.into_iter().next().unwrap()
            } else {
                let mut begin_list = vec![Value::Symbol("begin".to_string())];
                begin_list.extend(expanded_exprs);
                Value::List(begin_list)
            };

            println!("=== COMBINED PROGRAM ===");
            println!("Program: {:?}", non_cps_program);
            println!();

            // Compile to bytecode
            let module = compiler::compile(&non_cps_program, source.clone(), env.clone())?;

            println!("=== BYTECODE ASSEMBLY ===");
            for (i, instruction) in module.code.iter().enumerate() {
                println!("{:4}: {:?}", i, instruction);
            }
            println!("Constants: {:?}", module.constants);
            println!("Strings: {:?}", module.strings);
            println!();

            // Execute bytecode
            let mut vm = VM::new_with_env(env.clone(), false);
            let result = vm.execute(&module)?;

            println!("=== FINAL RESULT ===");
            println!("Non-CPS Bytecode: {:?}", result);
        }

        "superast" => {
            println!("=== PROCESSED AST DIRECT EVALUATION ===");

            // Load function prelude
            let source_with_prelude = samplescheme::builtins::inject_prelude(&source);

            // Parse the combined source (prelude + user code)
            let all_expressions = parser::parse_multiple(&source_with_prelude)?;

            // Expand macros for ProcessedAST compilation
            let mut macro_expander = MacroExpander::new(env.clone());
            macro_expander.load_prelude()?;

            let mut expanded_exprs = Vec::new();
            for expr in &all_expressions {
                let expanded = macro_expander.expand(expr)?;
                expanded_exprs.push(expanded);
            }

            println!("=== MACRO-EXPANDED AST ===");
            for (i, expr) in expanded_exprs.iter().enumerate() {
                println!("Expanded {}: {:?}", i, expr);
            }
            println!();

            // Compile to ProcessedAST using compile_multiple (no begin wrapper)
            println!("=== PROCESSED AST COMPILATION ===");
            let processed_ast = ProcessedAST::compile_multiple(&expanded_exprs)?;
            println!("ProcessedAST compiled successfully");
            println!();

            println!("=== PROCESSED AST DEBUG DUMP ===");
            println!("{}", processed_ast.debug_dump());
            println!();

            // Evaluate with SuperDirectVM
            let mut vm = SuperDirectVM::new(processed_ast);
            let env = ProcessedEnvironment::new();
            let result = vm.evaluate(Rc::new(env))?;

            println!("=== FINAL RESULT ===");
            println!("ProcessedAST Direct: {:?}", result);
        }

        "superstackast" => {
            println!("=== PROCESSED AST STACK EVALUATION ===");

            // Load function prelude
            let source_with_prelude = samplescheme::builtins::inject_prelude(&source);

            // Parse the combined source (prelude + user code)
            let all_expressions = parser::parse_multiple(&source_with_prelude)?;

            // Expand macros for ProcessedAST compilation
            let mut macro_expander = MacroExpander::new(env.clone());
            macro_expander.load_prelude()?;

            let mut expanded_exprs = Vec::new();
            for expr in &all_expressions {
                let expanded = macro_expander.expand(expr)?;
                expanded_exprs.push(expanded);
            }

            println!("=== MACRO-EXPANDED AST ===");
            for (i, expr) in expanded_exprs.iter().enumerate() {
                println!("Expanded {}: {:?}", i, expr);
            }
            println!();

            // Compile to ProcessedAST using compile_multiple (no begin wrapper)
            println!("=== PROCESSED AST COMPILATION ===");
            let processed_ast = ProcessedAST::compile_multiple(&expanded_exprs)?;
            println!("ProcessedAST compiled successfully");
            println!();

            println!("=== PROCESSED AST DEBUG DUMP ===");
            println!("{}", processed_ast.debug_dump());
            println!();

            // Evaluate with SuperStackVM
            let mut vm = SuperStackVM::new(processed_ast);
            let env = Rc::new(ProcessedEnvironment::new());
            let result = vm.evaluate(env)?;

            println!("=== FINAL RESULT ===");
            println!("ProcessedAST Result: {:?}", result);
        }

        "cps" => {
            println!("❌ CPS mode disabled due to stack overflow issues in macro expansion");
            println!(
                "Use 'ast', 'stackast', 'noncps', 'superast', or 'superstackast' modes instead."
            );
            std::process::exit(1);
        }

        _ => {
            eprintln!(
                "Invalid mode: {}. Use ast, stackast, noncps, superast, or superstackast",
                mode
            );
            std::process::exit(1);
        }
    }

    Ok(())
}
