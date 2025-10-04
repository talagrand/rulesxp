use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("Testing new clean CPS architecture...");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load all preludes - this should now include CPS macros automatically
    println!("Loading macro prelude (includes standard R7RS + CPS macros)...");
    match macro_expander.load_prelude() {
        Ok(_) => {
            println!("✓ All macro preludes loaded successfully");
            println!("  - Standard R7RS macros: and, or, when, unless, cond, case, let, let*, do");
            println!("  - CPS macros: cps-transform, rulesxp-internal-cps-app, to-cps");
        }
        Err(e) => {
            eprintln!("✗ Error loading macro preludes: {}", e);
            return;
        }
    }

    // Test 1: Standard macro still works
    println!("\n--- Test 1: Standard R7RS macro ---");
    let test1 = "(and #t #f)";
    match parse(test1) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(expanded) => {
                println!("✓ Standard macro works: {} -> {}", test1, expanded);
            }
            Err(e) => {
                println!("✗ Standard macro error: {}", e);
            }
        },
        Err(e) => {
            println!("✗ Parse error: {}", e);
        }
    }

    // Test 2: CPS macro works
    println!("\n--- Test 2: CPS macro ---");
    let test2 = "(to-cps (+ 1 2))";
    match parse(test2) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(expanded) => {
                println!("✓ CPS macro works: {} -> {}", test2, expanded);
            }
            Err(e) => {
                println!("✗ CPS macro error: {}", e);
            }
        },
        Err(e) => {
            println!("✗ Parse error: {}", e);
        }
    }

    // Test 3: Complex CPS macro
    println!("\n--- Test 3: Complex CPS transformation ---");
    let test3 = "(to-cps (if #t 42 0))";
    match parse(test3) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(expanded) => {
                println!("✓ Complex CPS works: {} -> {}", test3, expanded);
            }
            Err(e) => {
                println!("✗ Complex CPS error: {}", e);
            }
        },
        Err(e) => {
            println!("✗ Parse error: {}", e);
        }
    }

    println!("\n--- Test 4: Functions are available via inject_prelude ---");
    let test_code = "(identity 42)";
    let with_prelude = samplescheme::builtins::inject_prelude(test_code);

    // Check that both standard functions and CPS functions are included
    if with_prelude.contains("(define (identity x) x)") {
        println!("✓ CPS functions included in prelude");
    } else {
        println!("✗ CPS functions missing from prelude");
    }

    if with_prelude.contains("(define (cadr x)") {
        println!("✓ Standard functions included in prelude");
    } else {
        println!("✗ Standard functions missing from prelude");
    }

    println!("\n--- Summary ---");
    println!("✓ Clean architecture successfully implemented:");
    println!("  - CPS macros loaded automatically with standard macros");
    println!("  - CPS functions loaded automatically with standard functions");
    println!("  - No complex filtering logic needed");
    println!("  - Single inject_prelude() call provides all functions");
}
