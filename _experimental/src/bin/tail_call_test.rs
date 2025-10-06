use samplescheme::{
    macros::MacroExpander,
    parser,
    processed_ast::ProcessedAST,
    processed_env::ProcessedEnvironment,
    super_vm::SuperStackVM,
    value::{Environment, Value},
};
use std::rc::Rc;

fn test_tail_calls(program: &str, test_name: &str) -> Result<usize, Box<dyn std::error::Error>> {
    println!("Testing {}...", test_name);

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
    println!("=== TAIL CALL OPTIMIZATION TEST ===");

    // Test 1: Simple tail recursive countdown
    let program1 = r#"
        (define countdown
          (lambda (n)
            (if (<= n 0)
                0
                (countdown (- n 1)))))
        
        (countdown 1000)
    "#;

    let depth1 = test_tail_calls(program1, "tail recursive countdown from 1000")?;

    // Test 2: Mutual recursion (even/odd)
    let mutual_program = r#"
        (define is-even
          (lambda (n)
            (if (= n 0)
                #t
                (is-odd (- n 1)))))
                
        (define is-odd
          (lambda (n)
            (if (= n 0)
                #f
                (is-even (- n 1)))))
        
        (is-even 100)
    "#;

    let depth2 = test_tail_calls(mutual_program, "mutual recursion (even/odd) with n=100")?;

    // Test 3: Non-tail recursive function (should use more stack)
    let non_tail_program = r#"
        (define factorial
          (lambda (n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1))))))
        
        (factorial 10)
    "#;

    let depth3 = test_tail_calls(non_tail_program, "non-tail recursive factorial(10)")?;

    println!("\n=== TAIL CALL ANALYSIS ===");
    println!("Tail recursive countdown (1000): max depth = {}", depth1);
    println!("Mutual recursion (100): max depth = {}", depth2);
    println!("Non-tail factorial (10): max depth = {}", depth3);

    if depth1 < 20 {
        println!("✓ Tail call optimization appears to be working!");
        println!("  Countdown depth much smaller than expected without optimization");
    } else {
        println!("⚠ Tail call optimization may not be fully working");
        println!("  Expected depth < 20 for tail recursive countdown");
    }

    Ok(())
}
