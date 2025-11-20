/// Debug program to trace pattern matching
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Simpler test first - single clause without nested ellipsis
    let simple_macro = r#"(define-syntax simple-test (syntax-rules () ((_ (args ...) body) (lambda (args ...) body))))"#;
    let simple_expr = parser::parse(simple_macro).unwrap();
    let _ = expander.expand(&simple_expr)?;

    let simple_test = r#"(simple-test (proc lst) (+ proc lst))"#;
    let simple_parsed = parser::parse(simple_test).unwrap();
    println!("Simple Input: {:?}", simple_parsed);

    let simple_expanded = expander.expand(&simple_parsed)?;
    println!("Simple Expanded: {:?}", simple_expanded);

    Ok(())
}
