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

    // Test cases for ellipsis expansion - REMOVED: Now covered in tests/basic_expressions.scm
    println!("Ellipsis expansion tests moved to tests/basic_expressions.scm");
    println!("Run 'cargo test test_all_scheme_files' to see all test results.");
    
    // Keep the following as a simple demonstration of the macro expansion process only
    let demo_case = "(and #t #f)";
    println!("\nDemo: Macro expansion process for {}", demo_case);
    
    match parse(demo_case) {
        Ok(ast) => {
            println!("Original AST: {}", ast);
            match macro_expander.expand(&ast) {
                Ok(expanded_ast) => {
                    println!("Expanded AST: {}", expanded_ast);
                    match compile(&expanded_ast, demo_case.to_string(), vm.current_env()) {
                        Ok(module) => match vm.execute(&module) {
                            Ok(result) => {
                                println!("✓ Demo result: {}", result);
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

    println!("\nFor comprehensive ellipsis expansion testing, see tests/basic_expressions.scm");
}
