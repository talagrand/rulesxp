use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("Testing simple ellipsis binding...");

    let vm = VM::new();
    let mut expander = MacroExpander::new(vm.current_env());

    // Define a simple test macro
    let test_macro = "(define-syntax test-ellipsis
        (syntax-rules ()
            ((test-ellipsis x ...)
             (list x ...))))";

    match parse(test_macro) {
        Ok(ast) => match expander.expand(&ast) {
            Ok(_) => println!("Test macro defined successfully"),
            Err(e) => println!("Error defining macro: {}", e),
        },
        Err(e) => println!("Parse error: {}", e),
    }

    // Test the ellipsis expansion
    let test_usage = "(test-ellipsis a b c)";
    match parse(test_usage) {
        Ok(ast) => {
            println!("Input: {:?}", ast);
            match expander.expand(&ast) {
                Ok(result) => println!("Expanded: {:?}", result),
                Err(e) => println!("Expansion error: {}", e),
            }
        }
        Err(e) => println!("Parse error: {}", e),
    }
}
