/// Debug program to test the let macro that's causing lambda parameter errors
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Define the let macro
    let macro_def = r#"(define-syntax let
  (syntax-rules ()
    ((let name ((var val) ...) body ...)
     ((letrec ((name (lambda (var ...) body ...))) name) val ...))
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))"#;

    let macro_expr = parser::parse(macro_def).unwrap();
    let _ = expander.expand(&macro_expr)?;

    // Test a simple let expression
    let test_expr = r#"(let ((x 1) (y 2)) (+ x y))"#;
    let parsed = parser::parse(test_expr).unwrap();

    println!("Input: {:?}", parsed);

    let expanded = expander.expand(&parsed)?;
    println!("Expanded: {:?}", expanded);

    Ok(())
}
