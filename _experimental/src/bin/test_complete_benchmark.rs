use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Test the complete benchmark program
    let code = std::fs::read_to_string("benchmark_example.scm")?;

    // Create environment
    let env = Rc::new(Environment::new());

    // Parse
    let expressions = parser::parse_multiple(&code)?;

    // Create begin expression
    let begin_expr = if expressions.len() == 1 {
        expressions[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expressions);
        Value::List(begin_list)
    };

    // Macro expansion
    let mut macro_expander = MacroExpander::new(env.clone());
    macro_expander.load_prelude()?;
    let expanded = macro_expander.expand(&begin_expr)?;

    // Compile
    println!("Compiling...");
    let bytecode = compiler::compile(&expanded, code.to_string(), env.clone())?;

    // Execute
    println!("Executing...");
    let mut vm = VM::new_with_env(env.clone(), false);
    let result = vm.execute(&bytecode)?;

    println!("Result: {}", result);

    Ok(())
}
