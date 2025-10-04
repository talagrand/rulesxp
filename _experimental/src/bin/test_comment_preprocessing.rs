use samplescheme::parser::parse_multiple;

fn main() {
    println!("=== TESTING COMMENT PREPROCESSING ===\n");

    let test_cases = vec![
        // Basic comment removal
        (
            "; This is a comment\n(+ 1 2)",
            "Line comment before expression",
        ),
        (
            "(+ 1 2) ; This is an end-of-line comment\n(* 3 4)",
            "End-of-line comment",
        ),
        (
            r#"(define-syntax test
  (syntax-rules ()
    ;; This is an inline comment
    ((test x) x)))"#,
            "Comment inside syntax-rules",
        ),
        (
            r#"; Header comment
(define x 42) ; End comment
; Another line comment
(define y "hello ; not a comment")"#,
            "Mixed comments with string literal",
        ),
        (
            r#"(define-syntax cps-transform
  (syntax-rules ()
    ;; Self-evaluating literals - pass directly to continuation
    ((cps-transform val k)
     (k val))))"#,
            "CPS macro with inline comment",
        ),
    ];

    for (input, description) in test_cases {
        println!("Testing: {}", description);
        println!("Input:");
        println!("{}", input);
        println!();

        match parse_multiple(input) {
            Ok(expressions) => {
                println!("✓ Successfully parsed {} expressions:", expressions.len());
                for (i, expr) in expressions.iter().enumerate() {
                    println!("  {}: {}", i + 1, expr);
                }
            }
            Err(e) => {
                println!("✗ Parse failed: {}", e);
            }
        }
        println!("{}", "=".repeat(50));
    }

    // Test the specific CPS macros case that was failing
    println!("\nTesting original failing CPS macros:");
    let cps_macros = r#";; CPS transformation for basic expressions
(define-syntax cps-transform
  (syntax-rules ()
    ;; Self-evaluating literals - pass directly to continuation
    ((cps-transform val k) 
     (k val))))

;; Macro to convert a program to CPS with identity continuation
(define-syntax to-cps
  (syntax-rules ()
    ((to-cps expr)
     (cps-transform expr (lambda (result) result)))))"#;

    match parse_multiple(cps_macros) {
        Ok(expressions) => {
            println!(
                "✓ CPS macros parsed successfully ({} expressions)!",
                expressions.len()
            );
            for expr in expressions {
                println!("  {}", expr);
            }
        }
        Err(e) => {
            println!("✗ CPS macros still failing: {}", e);
        }
    }
}
