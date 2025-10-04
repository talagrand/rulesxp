use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("Testing simple CPS macro definition...");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load standard R7RS macros first
    if let Err(e) = macro_expander.load_prelude() {
        eprintln!("Error loading standard macro prelude: {}", e);
        return;
    }
    println!("✓ Standard macros loaded");

    // Test a simple macro definition directly
    println!("\n--- Testing simple to-cps macro definition ---");
    let simple_to_cps = "(define-syntax to-cps (syntax-rules () ((to-cps expr) expr)))";

    match parse(simple_to_cps) {
        Ok(ast) => {
            println!("Simple macro parsed: {}", ast);

            match macro_expander.expand(&ast) {
                Ok(_) => {
                    println!("✓ Simple to-cps macro loaded successfully");

                    // Test using the macro
                    let usage = "(to-cps (+ 1 2))";
                    match parse(usage) {
                        Ok(usage_ast) => match macro_expander.expand(&usage_ast) {
                            Ok(expanded) => {
                                println!("✓ Simple to-cps usage expanded: {}", expanded);
                            }
                            Err(e) => {
                                println!("✗ Usage expansion error: {}", e);
                            }
                        },
                        Err(e) => {
                            println!("✗ Usage parse error: {}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("✗ Simple macro expansion error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("✗ Simple macro parse error: {}", e);
        }
    }

    println!("\n--- Testing what the CPS prelude issue is ---");

    // Try parsing just the first macro from CPS prelude
    let cps_transform_simple = r#"(define-syntax test-cps 
        (syntax-rules ()
            ((test-cps val k) (k val))))"#;

    match parse(cps_transform_simple) {
        Ok(ast) => {
            println!("Simple CPS macro parsed: {}", ast);

            match macro_expander.expand(&ast) {
                Ok(_) => {
                    println!("✓ Simple CPS macro loaded");
                }
                Err(e) => {
                    println!("✗ Simple CPS macro error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("✗ Simple CPS macro parse error: {}", e);
        }
    }
}
