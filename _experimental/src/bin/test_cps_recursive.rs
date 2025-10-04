use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Test recursive function in CPS mode
    let code = r#"
(define factorial 
  (lambda (n) 
    (if (= n 0) 
        1 
        (* n (factorial (- n 1))))))

(factorial 5)
"#;

    println!("Testing recursive function in CPS mode...");

    // Create environment
    let env = Rc::new(Environment::new());

    // Parse
    let expressions = parser::parse_multiple(code)?;
    println!("Parsed {} expressions", expressions.len());

    // Create begin expression
    let begin_expr = if expressions.len() == 1 {
        expressions[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expressions);
        Value::List(begin_list)
    };

    // Macro expansion with CPS prelude
    let mut macro_expander = MacroExpander::new(env.clone());
    macro_expander.load_cps_prelude()?;
    let expanded = macro_expander.expand(&begin_expr)?;

    // Compile with CPS
    println!("Compiling with CPS...");
    let bytecode = compiler::compile_with_cps(&expanded, code.to_string(), env.clone())?;

    // Execute with CPS VM
    println!("Executing...");
    let mut vm = VM::new_with_cps(true);
    let result = vm.execute(&bytecode)?;

    println!("Result: {}", result);

    Ok(())
}
