use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("=== TESTING COMPLEX CPS TRANSFORMATIONS ===\n");

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

    // Test more specific cases to see if complex patterns work
    let advanced_test_cases = vec![
        // Test direct cps-transform usage (should be more specific)
        (
            "(cps-transform 42 identity)",
            "Direct CPS transform of literal",
        ),
        (
            "(cps-transform (quote hello) identity)",
            "Direct CPS transform of quote",
        ),
        (
            "(cps-transform (lambda (x) x) identity)",
            "Direct CPS transform of lambda",
        ),
        (
            "(cps-transform (if #t 42 0) identity)",
            "Direct CPS transform of if",
        ),
        // Test nested transformations
        ("(to-cps (begin 1 2 3))", "Begin sequence transformation"),
        (
            "(to-cps (if (> x 0) (+ x 1) (- x 1)))",
            "Nested if with applications",
        ),
        // Test with the helper macro
        (
            "(rulesxp-internal-cps-app + (1 2) () identity)",
            "Direct CPS app usage",
        ),
    ];

    for (input, description) in advanced_test_cases {
        println!("Testing {}: {}", description, input);
        match parse(input) {
            Ok(ast) => {
                println!("  Parsed: {}", ast);
                match macro_expander.expand(&ast) {
                    Ok(expanded) => {
                        println!("  ✓ Expanded: {}", expanded);

                        // Check if it's actually doing complex transformation
                        let expanded_str = expanded.to_string();
                        if expanded_str.contains("lambda") && expanded_str.contains("cps-transform")
                        {
                            println!("  🎯 Complex recursive transformation detected!");
                        } else if expanded_str != input {
                            println!("  📝 Basic transformation applied");
                        } else {
                            println!("  ⚠️  No transformation occurred");
                        }
                    }
                    Err(e) => {
                        println!("  ✗ Expansion failed: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("  ✗ Parse failed: {}", e);
            }
        }
        println!();
    }

    // Test if the issue is with pattern matching order
    println!("=== PATTERN MATCHING ANALYSIS ===");
    println!("The CPS macro has these patterns in order:");
    println!("1. (cps-transform val k) - catches all literals and variables");
    println!("2. (cps-transform (quote expr) k) - specific for quotes");
    println!("3. (cps-transform var k) - catches variables again");
    println!("4. (cps-transform (lambda ...) k) - specific for lambdas");
    println!("5. (cps-transform (if ...) k) - specific for if expressions");
    println!("6. ... more specific patterns");
    println!();
    println!("Issue: The first pattern might be too general and catching everything!");
}
