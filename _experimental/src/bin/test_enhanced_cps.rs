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

    let test_cases = vec![
        // Basic value transformation
        ("(to-cps 42)", "Number literal"),
        ("(to-cps #t)", "Boolean literal"),
        ("(to-cps x)", "Variable reference"),
        // Quote transformation
        ("(to-cps (quote hello))", "Quoted symbol"),
        ("(to-cps '(a b c))", "Quoted list"),
        // Lambda transformation
        ("(to-cps (lambda (x) x))", "Simple lambda"),
        ("(to-cps (lambda (x y) (+ x y)))", "Multi-arg lambda"),
        // If transformation
        ("(to-cps (if #t 42 0))", "Simple if expression"),
        (
            "(to-cps (if (> x 0) x (- x)))",
            "Complex if with expressions",
        ),
        // Application (should use basic rule)
        ("(to-cps (+ 1 2))", "Function application"),
        ("(to-cps (cons a b))", "Multi-arg application"),
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
