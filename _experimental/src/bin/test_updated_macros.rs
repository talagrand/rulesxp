use samplescheme::macros::MacroExpander;
use samplescheme::vm::VM;
use samplescheme::{parse, Value};

fn parse_and_expand(expander: &mut MacroExpander, code: &str) -> Result<Value, String> {
    match parse(code) {
        Ok(ast) => match expander.expand(&ast) {
            Ok(expanded) => Ok(expanded),
            Err(e) => Err(format!("Macro error: {}", e)),
        },
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

fn main() {
    println!("Testing stabilization-based macro expansion...");

    let vm = VM::new();
    let mut expander = MacroExpander::new(vm.current_env());

    // Test 1: Define a simple macro
    println!("\n1. Defining simple macro...");
    let simple_macro = "(define-syntax when
        (syntax-rules ()
            ((when test body) (if test body))))";

    match parse_and_expand(&mut expander, simple_macro) {
        Ok(result) => println!("   Expanded: {:?}", result),
        Err(e) => println!("   Error: {:?}", e),
    }

    // Test 2: Use the simple macro (stabilization-based expansion)
    println!("\n2. Using simple macro (stabilization-based expansion)...");
    println!("   Known macros before: {:?}", expander.known_macro_count());
    println!("   Note: Using per-expression stabilization instead of global tracking");

    let when_usage = "(when #t \"works\")";

    match parse_and_expand(&mut expander, when_usage) {
        Ok(result) => {
            println!("   Expanded: {:?}", result);
            println!("   Known macros after: {:?}", expander.known_macro_count());
            println!("   Note: Expansion stops when pre-expansion == post-expansion");
        }
        Err(e) => println!("   Error: {:?}", e),
    }

    // Test 3: Define a macro that generates another macro usage
    println!("\n3. Defining macro that generates macro usage...");
    let define_simple_macro = "(define-syntax define-simple-macro
        (syntax-rules ()
            ((define-simple-macro name replacement)
             (define-syntax name (syntax-rules () ((name) replacement))))))";

    match parse_and_expand(&mut expander, define_simple_macro) {
        Ok(result) => println!("   Expanded: {:?}", result),
        Err(e) => println!("   Error: {:?}", e),
    }

    // Test 4: Use complex macro (tests stabilization with nested expansions)
    println!("\n4. Using macro that generates another macro usage (tests stabilization)...");
    println!("   Known macros before: {:?}", expander.known_macro_count());
    println!("   Note: Short-circuit optimization skips AST comparison if no macros emitted");

    let complex_usage = "(define-simple-macro hello \"Hello World!\")";

    match parse_and_expand(&mut expander, complex_usage) {
        Ok(result) => {
            println!("   Expanded: {:?}", result);
            println!("   Known macros after: {:?}", expander.known_macro_count());
            println!("   Note: Stabilization handles recursive expansions automatically");
        }
        Err(e) => println!("   Error: {:?}", e),
    }
}
