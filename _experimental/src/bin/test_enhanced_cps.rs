use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("=== TESTING ENHANCED CPS MACROS ===\n");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load the prelude
    match macro_expander.load_prelude() {
        Ok(_) => println!("✓ Prelude loaded successfully"),
        Err(e) => {
            println!("✗ Prelude loading failed: {}", e);
            return;
        }
    }

    // Basic test cases REMOVED: Now covered in tests/*.scm files
    // Run 'cargo test test_all_scheme_files' to see comprehensive testing
    let test_cases = vec![
        // Keep only CPS-specific transformations that aren't easily testable in .scm files
        ("(to-cps (call/cc f))", "Call with current continuation"),
        (
            "(to-cps (letrec ((f (lambda (x) x))) (f 5)))",
            "Letrec transformation",
        ),
    ];

    for (input, description) in test_cases {
        println!("Testing {}: {}", description, input);
        match parse(input) {
            Ok(ast) => match macro_expander.expand(&ast) {
                Ok(expanded) => {
                    println!("  ✓ {} -> {}", input, expanded);
                }
                Err(e) => {
                    println!("  ✗ Expansion failed: {}", e);
                }
            },
            Err(e) => {
                println!("  ✗ Parse failed: {}", e);
            }
        }
        println!();
    }

    println!("=== CPS MACRO ANALYSIS ===");
    println!("The enhanced CPS macros now support:");
    println!("- Basic value pass-through");
    println!("- Quote expressions");
    println!("- Lambda conversion to CPS functions");
    println!("- If expressions with CPS transformation");
    println!("- Fallback to basic transformation for other expressions");
}
