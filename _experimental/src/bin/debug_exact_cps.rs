use samplescheme::parse;

fn main() {
    println!("=== EXACT CPS MACRO DEBUGGING ===\n");

    // Test the exact failing content from CPS macros
    let exact_cps_transform = r#"(define-syntax cps-transform
  (syntax-rules ()
    ;; Self-evaluating literals - pass directly to continuation
    ((cps-transform val k)
     (k val))))"#;

    println!("Testing exact CPS transform macro:");
    println!("Input:\n{}", exact_cps_transform);
    match parse(exact_cps_transform) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => {
            println!("✗ Failed: {}", e);

            // Try removing the comment
            let without_comment = r#"(define-syntax cps-transform
  (syntax-rules ()
    ((cps-transform val k)
     (k val))))"#;

            println!("\nTrying without comment:");
            println!("Input:\n{}", without_comment);
            match parse(without_comment) {
                Ok(ast) => println!("✓ Success without comment: {}", ast),
                Err(e2) => {
                    println!("✗ Still failed without comment: {}", e2);

                    // Try with single line comment style
                    let single_comment = r#"(define-syntax cps-transform
  (syntax-rules () ; comment here
    ((cps-transform val k)
     (k val))))"#;

                    println!("\nTrying with single line comment:");
                    match parse(single_comment) {
                        Ok(ast) => println!("✓ Success with single comment: {}", ast),
                        Err(e3) => println!("✗ Failed with single comment: {}", e3),
                    }
                }
            }
        }
    }

    // Test the to-cps macro
    let to_cps_macro = r#"(define-syntax to-cps
  (syntax-rules ()
    ((to-cps expr)
     (cps-transform expr (lambda (result) result)))))"#;

    println!("\n\nTesting to-cps macro:");
    println!("Input:\n{}", to_cps_macro);
    match parse(to_cps_macro) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }

    // Test if it's an issue with the combination
    let both_macros = format!("{}\n\n{}", exact_cps_transform, to_cps_macro);
    println!("\n\nTesting both macros together:");
    match parse(&both_macros) {
        Ok(ast) => println!("✓ Both work together: {}", ast),
        Err(e) => println!("✗ Failed together: {}", e),
    }
}
