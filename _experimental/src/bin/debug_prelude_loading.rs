/// Debug program to test prelude loading which might be causing lambda parameter errors
use samplescheme::macros::{MacroError, MacroExpander};
use samplescheme::parser;

fn main() -> Result<(), MacroError> {
    let mut expander = MacroExpander::new(Default::default());

    println!("Loading prelude...");

    // This is what causes the error according to the test runner
    match expander.load_prelude() {
        Ok(()) => {
            println!("Prelude loaded successfully!");

            // Now test a simple let expression that should work
            let test_expr = r#"(let ((x 1) (y 2)) (+ x y))"#;
            let parsed = parser::parse(test_expr).unwrap();

            println!("Testing simple let expression...");
            println!("Input: {:?}", parsed);

            let expanded = expander.expand(&parsed)?;
            println!("Expanded: {:?}", expanded);
        }
        Err(e) => {
            println!("Error loading prelude: {:?}", e);
        }
    }

    Ok(())
}
