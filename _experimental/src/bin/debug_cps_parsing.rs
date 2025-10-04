use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("Testing CPS macro pieces individually...");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load standard R7RS macros first
    if let Err(e) = macro_expander.load_prelude() {
        eprintln!("Error loading standard macro prelude: {}", e);
        return;
    }

    // Test 1: Simple cps-transform with just one rule
    println!("\n--- Test 1: Simple cps-transform (one rule) ---");
    let simple_cps_transform = r#"(define-syntax cps-transform-simple
        (syntax-rules ()
            ((cps-transform-simple val k) (k val))))"#;

    match parse(simple_cps_transform) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ Simple cps-transform works"),
            Err(e) => println!("✗ Simple cps-transform error: {}", e),
        },
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 2: cps-transform with literals list (empty)
    println!("\n--- Test 2: cps-transform with empty literals ---");
    let cps_with_literals = r#"(define-syntax cps-transform-literals
        (syntax-rules ()
            ((cps-transform-literals val k) (k val))))"#;

    match parse(cps_with_literals) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ CPS with empty literals works"),
            Err(e) => println!("✗ CPS with empty literals error: {}", e),
        },
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 3: cps-transform with actual literals (this might be the problem)
    println!("\n--- Test 3: cps-transform with literals list ---");
    let cps_with_real_literals = r#"(define-syntax cps-transform-with-literals
        (syntax-rules (lambda if)
            ((cps-transform-with-literals val k) (k val))))"#;

    match parse(cps_with_real_literals) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ CPS with real literals works"),
            Err(e) => println!("✗ CPS with real literals error: {}", e),
        },
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 4: Multiple rules (this is complex too)
    println!("\n--- Test 4: Multiple rules ---");
    let multiple_rules = r#"(define-syntax cps-multi
        (syntax-rules ()
            ((cps-multi val k) (k val))
            ((cps-multi (quote expr) k) (k (quote expr)))))"#;

    match parse(multiple_rules) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ Multiple rules work"),
            Err(e) => println!("✗ Multiple rules error: {}", e),
        },
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 5: Recursive macro call (this is very complex)
    println!("\n--- Test 5: Recursive macro patterns ---");
    let recursive_macro = r#"(define-syntax cps-recursive
        (syntax-rules ()
            ((cps-recursive (begin expr) k) (cps-recursive expr k))
            ((cps-recursive val k) (k val))))"#;

    match parse(recursive_macro) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ Recursive patterns work"),
            Err(e) => println!("✗ Recursive patterns error: {}", e),
        },
        Err(e) => println!("✗ Parse error: {}", e),
    }
}
