/// Minimal test to debug ellipsis binding issue
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Test the simplest possible ellipsis pattern that might fail
    let macro_def = r#"(define-syntax test-simple (syntax-rules () ((_ x ...) (begin x ...))))"#;
    let macro_expr = parser::parse(macro_def).unwrap();
    let _ = expander.expand(&macro_expr)?;

    // Test with simple input that should work
    let test_expr = r#"(test-simple a b c)"#;
    let parsed = parser::parse(test_expr).unwrap();
    println!("Input: {:?}", parsed);

    let expanded = expander.expand(&parsed)?;
    println!("Expanded: {:?}", expanded);

    Ok(())
}
