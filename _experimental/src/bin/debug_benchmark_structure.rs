use samplescheme::{
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
};
use std::rc::Rc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple test to see what gets parsed
    let code = std::fs::read_to_string("benchmark_example.scm")?;

    println!("=== PARSING TEST ===");
    let expressions = parser::parse_multiple(&code)?;
    println!("Parsed {} expressions:", expressions.len());
    for (i, expr) in expressions.iter().enumerate() {
        println!("Expression {}: {}", i, expr);
    }

    println!("\n=== BEGIN EXPRESSION TEST ===");
    let begin_expr = if expressions.len() == 1 {
        expressions[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(expressions);
        Value::List(begin_list)
    };

    println!("Begin expression: {}", begin_expr);

    println!("\n=== MACRO EXPANSION TEST ===");
    let env = Rc::new(Environment::new());
    let mut macro_expander = MacroExpander::new(env.clone());
    macro_expander.load_prelude()?;

    let expanded = macro_expander.expand(&begin_expr)?;
    println!("Expanded: {}", expanded);

    Ok(())
}
