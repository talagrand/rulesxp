/// Debug program to test nested ellipsis without prelude
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Test the original problematic nested ellipsis pattern
    let macro_def = r#"(define-syntax debug-nested-ellipsis (syntax-rules () ((_ ((args ...) body) ...) ((lambda (args ...) body) ...))))"#;

    let macro_expr = parser::parse(macro_def).unwrap();
    let _ = expander.expand(&macro_expr)?;

    // Test the macro
    let test_expr = r#"(debug-nested-ellipsis ((x y) (+ x y)) ((a b c) (list a b c)))"#;
    let parsed = parser::parse(test_expr).unwrap();
    println!("Input: {:?}", parsed);

    let expanded = expander.expand(&parsed)?;
    println!("Expanded: {:?}", expanded);

    // Test simpler case to understand the pattern matching
    println!("\n=== SIMPLE TEST ===");
    let simple_test = r#"(debug-nested-ellipsis ((a b) (+ a b)))"#;
    let simple_parsed = parser::parse(simple_test).unwrap();
    println!("Simple input: {:?}", simple_parsed);

    let simple_expanded = expander.expand(&simple_parsed)?;
    println!("Simple expanded: {:?}", simple_expanded);

    Ok(())
}
