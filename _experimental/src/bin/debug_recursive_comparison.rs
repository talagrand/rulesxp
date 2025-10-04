use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Debug why benchmark fails but simple test works
    let simple_code = r#"
(define factorial 
  (lambda (n) 
    (if (= n 0) 
        1 
        (* n (factorial (- n 1))))))

(factorial 5)
"#;

    let benchmark_code = std::fs::read_to_string("benchmark_example.scm")?;

    println!("=== TESTING SIMPLE RECURSIVE FUNCTION ===");
    test_code(simple_code, "simple")?;

    println!("\n=== TESTING FULL BENCHMARK CODE ===");
    test_code(&benchmark_code, "benchmark")?;

    Ok(())
}

fn test_code(code: &str, name: &str) -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing {} code...", name);

    // Create environment - same as benchmark
    let env = Rc::new(Environment::new());

    // Parse
    let expressions = parser::parse_multiple(code)?;
    println!("Parsed {} expressions", expressions.len());

    // Macro expansion with non-CPS prelude - same as benchmark
    let mut macro_expander = MacroExpander::new(env.clone());
    macro_expander.load_prelude()?;

    let mut expanded_exprs = Vec::new();
    for expr in expressions {
        let expanded = macro_expander.expand(&expr)?;
        expanded_exprs.push(expanded);
    }

    // Create begin expression - same as benchmark
    let begin_expr = if expanded_exprs.len() == 1 {
        expanded_exprs[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expanded_exprs);
        Value::List(begin_list)
    };

    // Compile - same as benchmark
    println!("Compiling...");
    let bytecode = compiler::compile(&begin_expr, code.to_string(), env.clone())?;

    // Execute - same as benchmark
    println!("Executing...");
    let mut vm = VM::new_with_cps(false);
    let result = vm.execute(&bytecode)?;

    println!("Result: {}", result);

    Ok(())
}
