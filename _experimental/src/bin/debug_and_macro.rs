use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("Debugging macro expansion for (and #t #f)...");

    let vm = VM::new();
    let mut expander = MacroExpander::new(vm.current_env());

    // Load prelude
    if let Err(e) = expander.load_prelude() {
        println!("Failed to load prelude: {}", e);
        return;
    }

    let test_case = "(and #t #f)";
    println!("Input: {}", test_case);

    match parse(test_case) {
        Ok(ast) => {
            println!("Parsed AST: {:?}", ast);

            match expander.expand(&ast) {
                Ok(result) => {
                    println!("Expanded result: {:?}", result);
                }
                Err(e) => {
                    println!("Expansion error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}
