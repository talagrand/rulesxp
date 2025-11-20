/// Debug program to reproduce the lambda parameter expansion bug
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Create a macro that will cause the exact pattern from the error
    // Let's try to force the bug by having multiple nested ellipsis levels
    let macro_def = r#"(define-syntax bad-lambda-test
      (syntax-rules ()
        ((bad-lambda-test ((var ...) ...) body)
         (lambda (var ... ...) body))))"#;

    let macro_expr = parser::parse(macro_def).unwrap();
    let _ = expander.expand(&macro_expr)?;

    // Test the pattern that seems to be causing the issue
    let test_expr = r#"(bad-lambda-test ((proc lst) (a b c)) (+ 1 2))"#;
    let parsed = parser::parse(test_expr).unwrap();

    println!("Input: {:?}", parsed);

    let expanded = expander.expand(&parsed)?;
    println!("Expanded: {:?}", expanded);

    Ok(())
}
