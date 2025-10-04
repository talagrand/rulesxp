use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("=== CONCRETE EXAMPLES OF MACRO LIMITATIONS ===\n");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    match macro_expander.load_prelude() {
        Ok(_) => println!("✓ Prelude loaded successfully"),
        Err(e) => {
            println!("✗ Prelude loading failed: {}", e);
            return;
        }
    }

    println!("\n=== NESTED ELLIPSIS DETAILED BREAKDOWN ===");

    // Demonstrate the specific problem
    println!("1. What R7RS requires for do loops:");
    println!("   Pattern: ((var init step-expr) ...)");
    println!("   Where step-expr is OPTIONAL");
    println!();
    println!("   Examples:");
    println!("   - ((i 0 (+ i 1))) - with step");
    println!("   - ((i 0)) - without step");
    println!("   - ((i 0 (+ i 1)) (sum 0 (+ sum i))) - multiple with steps");
    println!();

    // Show the actual pattern that would be needed
    let full_do_pattern = r#"(define-syntax do-r7rs
  (syntax-rules ()
    ((do-r7rs ((var init) ...) (test result ...) body ...)
     ;; Simple case - no steps
     (let loop ((var init) ...)
       (if test (begin result ...) (begin body ... (loop var ...)))))
    ((do-r7rs ((var init step) ...) (test result ...) body ...)
     ;; Complex case - with steps (THIS FAILS - NESTED ELLIPSIS)
     (let loop ((var init) ...)
       (if test (begin result ...) (begin body ... (loop step ...)))))))"#;

    println!("2. Attempting to define full R7RS do:");
    match parse(&full_do_pattern.replace('\n', " ").replace("     ", " ")) {
        Ok(_) => println!("   ✓ Parses successfully"),
        Err(e) => println!("   ❌ Parse fails: {}", e),
    }

    // Test with simpler nested pattern
    let simple_nested = "(define-syntax test-nested (syntax-rules () ((test-nested ((a b) ...) result) (quote nested))))";
    println!("\n3. Even simpler nested ellipsis:");
    match parse(simple_nested) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("   ✓ Simple nested pattern works"),
            Err(e) => println!("   ❌ Simple nested fails: {}", e),
        },
        Err(e) => println!("   ❌ Parse fails: {}", e),
    }

    // Test with truly nested ellipsis
    let true_nested = "(define-syntax test-double (syntax-rules () ((test-double ((a b ...) ...) result) (quote double-nested))))";
    println!("\n4. True nested ellipsis ((a b ...) ...):");
    match parse(true_nested) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("   ✓ Double nested pattern works"),
            Err(e) => println!("   ❌ Double nested fails: {}", e),
        },
        Err(e) => println!("   ❌ Parse fails: {}", e),
    }

    println!("\n=== COMPLEX PATTERN MATCHING ISSUES ===");

    // Show a realistic CPS transformation that would need complex patterns
    println!("5. Advanced CPS transformations need many specific patterns:");
    println!("   - Different lambda arities: (), (x), (x y), (x y z), ...");
    println!("   - Different if forms: (if test), (if test then), (if test then else)");
    println!("   - Different binding forms: let, let*, letrec with various arities");
    println!("   - Application forms: (f), (f a), (f a b), (f a b c), ...");

    // Test macro with many patterns (this should work but demonstrates complexity)
    let many_patterns = r#"(define-syntax complex-cps
  (syntax-rules (lambda let if)
    ((complex-cps (lambda () body) k) (k (lambda (cont) (complex-cps body cont))))
    ((complex-cps (lambda (a) body) k) (k (lambda (cont a) (complex-cps body cont))))
    ((complex-cps (lambda (a b) body) k) (k (lambda (cont a b) (complex-cps body cont))))
    ((complex-cps (if test then else) k) (complex-cps test (lambda (t) (if t (complex-cps then k) (complex-cps else k)))))
    ((complex-cps (if test then) k) (complex-cps test (lambda (t) (if t (complex-cps then k) (k #f)))))
    ((complex-cps (let ((var val)) body) k) (complex-cps val (lambda (v) (let ((var v)) (complex-cps body k)))))
    ((complex-cps atom k) (k atom))))"#;

    println!("\n6. Testing complex CPS with many patterns:");
    match parse(&many_patterns.replace('\n', " ").replace("    ", " ")) {
        Ok(ast) => {
            match macro_expander.expand(&ast) {
                Ok(_) => {
                    println!("   ✓ Complex CPS macro definition succeeds");

                    // Test some expansions
                    test_cps_expansion(&mut macro_expander, "(complex-cps 42 identity)", "Atom");
                    test_cps_expansion(
                        &mut macro_expander,
                        "(complex-cps (lambda () 42) identity)",
                        "Zero-arg lambda",
                    );
                    test_cps_expansion(
                        &mut macro_expander,
                        "(complex-cps (lambda (x) x) identity)",
                        "One-arg lambda",
                    );
                    test_cps_expansion(
                        &mut macro_expander,
                        "(complex-cps (if #t 1 2) identity)",
                        "If expression",
                    );
                }
                Err(e) => println!("   ❌ Complex CPS fails: {}", e),
            }
        }
        Err(e) => println!("   ❌ Parse fails: {}", e),
    }

    println!("\n=== WHAT BREAKS WITH CURRENT LIMITATIONS ===");

    // Show specific constructs that don't work
    println!("7. Constructs blocked by nested ellipsis limitation:");
    println!("   ❌ (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ...)");
    println!("   ❌ (let-values (((a b) (values 1 2)) ((c d) (values 3 4))) body)");
    println!("   ❌ (define-values (a b c) (values 1 2 3))");
    println!("   ❌ (match expr ((pattern vars ...) body) ...)");
    println!("   ❌ (syntax-case expr (literals ...) (pattern template) ...)");

    println!("\n8. Advanced CPS transformations that would fail:");
    println!("   ❌ CPS transformation of full do loops");
    println!("   ❌ CPS transformation of let-values");
    println!("   ❌ CPS transformation of match expressions");
    println!("   ❌ Generic CPS for variable-arity functions");

    // Show what the error looks like
    println!("\n9. The actual error message:");
    let failing_pattern =
        "(define-syntax fail-test (syntax-rules () ((fail-test ((a b ...) ...) result) result)))";
    match parse(failing_pattern) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(_) => println!("   Unexpectedly succeeded!"),
            Err(e) => println!("   Error: {}", e),
        },
        Err(e) => println!("   Parse error: {}", e),
    }

    println!(
        r#"
=== CONCLUSION ===

The nested ellipsis limitation blocks:

1. **Full R7RS compliance** - Can't implement standard constructs
2. **Advanced macro programming** - Can't write generic macros  
3. **Complex CPS transformations** - Can't handle variable binding forms
4. **Meta-programming** - Can't write macros that generate complex patterns

This is why our CPS system, while syntactically correct, cannot handle
the full range of Scheme expressions that would need CPS transformation
in a complete implementation.

The workaround is to implement specific, limited versions of complex
constructs (like our simplified 'do' macro) rather than the full
R7RS-compliant versions.
"#
    );
}

fn test_cps_expansion(macro_expander: &mut MacroExpander, input: &str, description: &str) {
    print!("     {}: ", description);
    match parse(input) {
        Ok(ast) => match macro_expander.expand(&ast) {
            Ok(expanded) => {
                let expanded_str = format!("{}", expanded);
                if expanded_str.len() > 60 {
                    println!("✓ Success (expansion too long to show)");
                } else {
                    println!("✓ {} -> {}", input, expanded);
                }
            }
            Err(e) => println!("❌ Failed: {}", e),
        },
        Err(e) => println!("❌ Parse failed: {}", e),
    }
}
