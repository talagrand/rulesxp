use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("=== DEMONSTRATING MACRO LIMITATIONS WITH EXAMPLES ===\n");

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

    println!("\n=== 1. NESTED ELLIPSIS PROBLEMS ===");
    println!("R7RS `do` syntax: (do ((var init step) ...) (test expr ...) command ...)");
    println!(
        "This requires nested ellipsis: the pattern ((var init step) ...) has ellipsis inside ellipsis"
    );

    // Show what fails - simplified nested ellipsis pattern
    let nested_pattern = "(define-syntax test-nested (syntax-rules () ((test-nested ((a b ...) ...) result) result)))";
    test_single_macro(
        &mut macro_expander,
        nested_pattern,
        "Nested ellipsis pattern",
    );

    // Show what works - single level ellipsis
    let single_pattern =
        "(define-syntax test-single (syntax-rules () ((test-single (a ...) result) result)))";
    test_single_macro(
        &mut macro_expander,
        single_pattern,
        "Single ellipsis pattern",
    );

    println!("\n=== 2. COMPLEX PATTERN MATCHING ISSUES ===");

    // Example: Multiple lambda patterns with different arities
    println!("Problem: CPS needs different patterns for different lambda arities:");
    println!("  (lambda () body) vs (lambda (x) body) vs (lambda (x y) body)");

    let multi_lambda = "(define-syntax cps-lambda (syntax-rules (lambda) ((cps-lambda (lambda () body) k) (k 'zero-args)) ((cps-lambda (lambda (x) body) k) (k 'one-arg)) ((cps-lambda (lambda (x y) body) k) (k 'two-args)) ((cps-lambda expr k) (k 'other))))";
    test_single_macro(
        &mut macro_expander,
        multi_lambda,
        "Multiple lambda patterns",
    );

    if test_expansion(
        &mut macro_expander,
        "(cps-lambda (lambda () 42) identity)",
        "Zero-arg lambda",
    ) {
        test_expansion(
            &mut macro_expander,
            "(cps-lambda (lambda (x) x) identity)",
            "One-arg lambda",
        );
        test_expansion(
            &mut macro_expander,
            "(cps-lambda (lambda (x y) x) identity)",
            "Two-arg lambda",
        );
    }

    println!("\n=== 3. REAL WORLD EXAMPLE: R7RS DO LOOP ===");
    println!("Standard R7RS do: (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((> i 10) sum) (display i))");
    println!("Problem: ((var init step) ...) - each binding can have optional step");

    // Show our simplified version vs what R7RS requires
    println!("\n✅ Our working simplified version:");
    test_expansion(
        &mut macro_expander,
        "(do ((i 0)) ((> i 5) i) (display i))",
        "Simplified do",
    );

    println!("\n❌ What R7RS requires (would fail to parse):");
    println!("  (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((> i 10) sum) (display i))");
    println!("  This needs: ((var init step-expr) ...)");
    println!("  Where step-expr is optional, creating nested ellipsis complexity");

    println!("\n=== 4. CPS TRANSFORMATION COMPLEXITY ===");
    println!("CPS needs to handle many expression types with proper continuation threading:");

    // Test current CPS macro
    test_expansion(
        &mut macro_expander,
        "(cps-transform (lambda (x) x) identity)",
        "CPS lambda transform",
    );
    test_expansion(
        &mut macro_expander,
        "(cps-transform (if #t 42 0) identity)",
        "CPS if transform",
    );
    test_expansion(
        &mut macro_expander,
        "(cps-transform (begin 1 2 3) identity)",
        "CPS begin transform",
    );

    println!("\n❌ What would fail with complex CPS:");
    println!("  (cps-transform (do ((i 0 (+ i 1))) ((> i 5) i) (display i)) identity)");
    println!(
        "  This would require CPS transformation of do loops, which need nested ellipsis support"
    );

    println!("\n=== 5. WORKAROUNDS AND SOLUTIONS ===");
    println!("Current workarounds for limitations:");
    println!("✅ Simplified patterns: Use fixed-arity patterns instead of variable patterns");
    println!("✅ Multiple macro definitions: Split complex macros into simpler ones");
    println!("✅ Manual ellipsis expansion: Handle repetition in template expansion code");

    // Demonstrate workaround
    let workaround_do = "(define-syntax do-simple (syntax-rules () ((do-simple ((var init)) (test result) body) (let loop ((var init)) (if test result (begin body (loop var)))))))";
    test_single_macro(
        &mut macro_expander,
        workaround_do,
        "Workaround: simplified do",
    );

    println!(
        r#"
=== SUMMARY OF LIMITATIONS ===

1. **Nested Ellipsis**: Pattern ((a b ...) ...) fails
   - Blocks full R7RS do loops
   - Blocks variable-arity function definitions
   - Blocks complex binding forms

2. **Pattern Complexity**: Too many pattern alternatives cause issues
   - Multiple lambda arities in single macro
   - Context-dependent transformations
   - Deep nesting with many cases

3. **Template Expansion**: Complex template generation has bugs
   - Variable binding across ellipsis levels
   - Proper nesting of generated code
   - Scope management in complex expansions

4. **Real Impact**:
   - ❌ Full R7RS do loops
   - ❌ Advanced CPS transformations  
   - ❌ Generic programming constructs
   - ❌ Complex binding forms (letrec with steps)
   - ✅ Simple derived expressions (and, or, when, unless)
   - ✅ Basic binding forms (let, let*)
   - ✅ Simple CPS transformations
"#
    );
}

fn test_single_macro(macro_expander: &mut MacroExpander, macro_def: &str, description: &str) {
    print!("Testing {}: ", description);
    match parse(macro_def) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("✓ Parses and expands"),
            Err(e) => println!("❌ Expansion failed: {}", e),
        },
        Err(e) => println!("❌ Parse failed: {}", e),
    }
}

fn test_expansion(macro_expander: &mut MacroExpander, input: &str, description: &str) -> bool {
    print!("  {}: ", description);
    match parse(input) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(expanded) => {
                println!("✓ {} -> {}", input, expanded);
                true
            }
            Err(e) => {
                println!("❌ Failed: {}", e);
                false
            }
        },
        Err(e) => {
            println!("❌ Parse failed: {}", e);
            false
        }
    }
}
