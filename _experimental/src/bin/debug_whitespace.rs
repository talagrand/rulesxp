use samplescheme::parse;

fn main() {
    println!("=== MINIMAL WHITESPACE DEBUGGING ===\n");

    // Test 1: Single line version (works)
    let single_line = "(define-syntax test (syntax-rules () ((test x) x)))";
    println!("Testing single line:");
    println!("Input: {}", single_line);
    match parse(single_line) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }

    // Test 2: Multi-line version (fails)
    let multi_line = r#"(define-syntax test
  (syntax-rules ()
    ((test x) x)))"#;
    println!("\nTesting multi-line:");
    println!("Input: {}", multi_line);
    match parse(multi_line) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }

    // Test 3: Multi-line with different spacing
    let multi_line_2 = "(define-syntax test\n  (syntax-rules ()\n    ((test x) x)))";
    println!("\nTesting multi-line variant 2:");
    println!("Input: {}", multi_line_2);
    match parse(multi_line_2) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }

    // Test 4: Just the problematic part
    let syntax_rules_part = r#"(syntax-rules ()
    ((test x) x))"#;
    println!("\nTesting just syntax-rules part:");
    println!("Input: {}", syntax_rules_part);
    match parse(syntax_rules_part) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }

    // Test 5: Simplest multi-line list
    let simple_list = r#"(a
  b)"#;
    println!("\nTesting simplest multi-line list:");
    println!("Input: {}", simple_list);
    match parse(simple_list) {
        Ok(ast) => println!("✓ Success: {}", ast),
        Err(e) => println!("✗ Failed: {}", e),
    }
}
