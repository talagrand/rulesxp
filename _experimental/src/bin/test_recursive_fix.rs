use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple test of recursive function
    let code = r#"
(define factorial 
  (lambda (n) 
    (if (= n 0) 
        1 
        (* n (factorial (- n 1))))))

(factorial 5)
"#;

    println!("Testing recursive function compilation and execution...");

    // Create environment
    let env = Rc::new(Environment::new());

    // Parse
    let expressions = parser::parse_multiple(code)?;
    println!("Parsed {} expressions", expressions.len());

    // Macro expansion - use standard prelude for non-CPS testing
    let mut macro_expander = MacroExpander::new(env.clone());
    macro_expander.load_prelude()?;

    let mut expanded_exprs = Vec::new();
    for expr in expressions {
        let expanded = macro_expander.expand(&expr)?;
        expanded_exprs.push(expanded);
    }

    // Create a begin expression
    let begin_expr = if expanded_exprs.len() == 1 {
        expanded_exprs[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expanded_exprs);
        Value::List(begin_list)
    };

    // Compile non-CPS
    println!("Compiling non-CPS...");
    let bytecode = compiler::compile(&begin_expr, code.to_string(), env.clone())?;

    // Execute
    println!("Executing...");
    let mut vm = VM::new_with_cps(false); // Use non-CPS mode
    let result = vm.execute(&bytecode)?;

    println!("Result: {}", result);

    Ok(())
}
