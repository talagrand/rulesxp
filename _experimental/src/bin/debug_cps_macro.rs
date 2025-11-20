/// Debug program to test the CPS macro that's causing lambda parameter errors
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    // Define the CPS macro
    let macro_def = r#"(define-syntax cps-multi
      (syntax-rules (lambda if)
        ((cps-multi (lambda () body) k)
         (k (lambda (cont) (cps-multi body cont))))
        ((cps-multi (lambda (x) body) k) 
         (k (lambda (cont x) (cps-multi body cont))))
        ((cps-multi (lambda (x y) body) k)
         (k (lambda (cont x y) (cps-multi body cont))))
        ((cps-multi (if test) k)
         (cps-multi test (lambda (t) (if t (k #t) (k #f)))))
        ((cps-multi (if test then) k)
         (cps-multi test (lambda (t) (if t (cps-multi then k) (k #f)))))
        ((cps-multi (if test then else) k)
         (cps-multi test (lambda (t) (if t (cps-multi then k) (cps-multi else k)))))
        ((cps-multi expr k) (k expr))))"#;

    let macro_expr = parser::parse(macro_def).unwrap();
    let _ = expander.expand(&macro_expr)?;

    // Test the simplest case
    let test_expr = r#"(cps-multi (lambda (x) x) (lambda (f) f))"#;
    let parsed = parser::parse(test_expr).unwrap();

    println!("Input: {:?}", parsed);

    let expanded = expander.expand(&parsed)?;
    println!("Expanded: {:?}", expanded);

    Ok(())
}
