use samplescheme::macros::MacroExpander;
use samplescheme::{compile, parse, VM};

fn main() {
    println!("Testing ellipsis macro expansion...");

    let mut vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load the macro prelude first
    match macro_expander.load_prelude() {
        Ok(_) => println!("✓ Prelude loaded successfully"),
        Err(e) => {
            println!("✗ Failed to load prelude: {}", e);
            return;
        }
    }

    // Test cases for ellipsis expansion
    let test_cases = vec![
        ("(and)", "#t"),
        ("(and #t)", "#t"),
        ("(and #t #f)", "#f"),
        ("(and #t #t #t)", "#t"),
        ("(or)", "#f"),
        ("(or #f)", "#f"),
        ("(or #f #t)", "#t"),
        ("(or #f #f #t)", "#t"),
    ];

    for (i, (input, expected)) in test_cases.iter().enumerate() {
        println!("\nTest {}: {}", i + 1, input);

        match parse(input) {
            Ok(ast) => {
                // Expand macros first
                match macro_expander.expand(&ast) {
                    Ok(expanded_ast) => {
                        match compile(&expanded_ast, input.to_string(), vm.current_env()) {
                            Ok(module) => match vm.execute(&module) {
                                Ok(result) => {
                                    let result_str = format!("{}", result);
                                    if result_str == *expected {
                                        println!("✓ Expected: {}, Got: {}", expected, result_str);
                                    } else {
                                        println!("✗ Expected: {}, Got: {}", expected, result_str);
                                    }
                                }
                                Err(e) => println!("✗ Runtime error: {}", e),
                            },
                            Err(e) => println!("✗ Compile error: {}", e),
                        }
                    }
                    Err(e) => println!("✗ Macro expansion error: {}", e),
                }
            }
            Err(e) => println!("✗ Parse error: {}", e),
        }
    }

    println!("\nEllipsis expansion test complete!");
}
