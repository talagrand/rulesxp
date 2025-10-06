use samplescheme::{
    macros::MacroExpander,
    parser,
    processed_ast::ProcessedAST,
    processed_env::ProcessedEnvironment,
    super_vm::SuperStackVM,
    value::{Environment, Value},
};
use std::rc::Rc;

fn test_simple_tail_call() -> Result<usize, Box<dyn std::error::Error>> {
    println!("Testing simple tail call...");

    // Simple tail recursive function
    let program = r#"
        (define countdown
          (lambda (n)
            (if (<= n 0)
                n
                (countdown (- n 1)))))
        
        (countdown 100)
    "#;

    // Parse the program
    let expressions = parser::parse_multiple(program)?;
    println!("Parsed {} expressions", expressions.len());

    // Create environment with builtins
    let env = {
        let global_env = Environment::new();
        let builtins = samplescheme::builtins::create_builtins();
        for (name, value) in builtins {
            global_env.define(name, value);
        }
        Rc::new(global_env)
    };

    // Expand macros
    let mut expander = MacroExpander::new(env.clone());
    let mut expanded_expressions = Vec::new();
    for expr in expressions {
        let expanded = expander.expand(&expr)?;
        expanded_expressions.push(expanded);
    }

    // Combine into single program
    let mut begin_list = vec![Value::Symbol("begin".to_string())];
    begin_list.extend(expanded_expressions);
    let combined_program = Value::List(begin_list);

    // Create ProcessedAST
    let compiled_ast = ProcessedAST::compile(&combined_program)?;
    let mut super_vm = SuperStackVM::new(compiled_ast);

    // Create processed environment
    let processed_env = ProcessedEnvironment::new();
    let processed_env_rc = Rc::new(processed_env);

    // Evaluate
    let result = super_vm.evaluate(processed_env_rc)?;

    println!("SuperStackVM result: {:?}", result);
    let max_depth = super_vm.get_max_stack_depth();
    println!("Max stack depth during execution: {}", max_depth);

    Ok(max_depth)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== TAIL CALL OPTIMIZATION VERIFICATION ===");

    let depth = test_simple_tail_call()?;

    println!("\n=== RESULTS ===");
    if depth < 20 {
        println!("✅ Tail call optimization is working!");
        println!(
            "   Stack depth: {} (much less than 100 expected without optimization)",
            depth
        );
    } else {
        println!("❌ Tail call optimization may not be working");
        println!(
            "   Stack depth: {} (expected < 20 with optimization)",
            depth
        );
    }

    Ok(())
}
