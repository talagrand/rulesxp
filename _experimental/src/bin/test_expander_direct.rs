// Direct test of macro_expander with known-good bindings from matcher
//
// This tests the expander in isolation using validated bindings from the matcher

use samplescheme::macro_compiler::{MacroError, parse_syntax_rules};
use samplescheme::macro_expander::expand_template;
use samplescheme::macro_matcher::{MatchResult, match_pattern};
use samplescheme::parser::{parse, parse_multiple};
use samplescheme::value::Value;

fn main() {
    println!("=== MACRO EXPANDER DIRECT TEST ===\n");

    // Test 1: Simple variable substitution (depth 0)
    test_simple_variable();

    // Test 2: Single ellipsis expansion (depth 1)
    test_single_ellipsis();

    // Test 3: Nested ellipsis expansion (depth 2)
    test_nested_ellipsis();

    // Test 4: Empty ellipsis
    test_empty_ellipsis();

    // Test 5: Multiple variables in ellipsis
    test_multiple_vars_ellipsis();

    println!("\n=== ALL TESTS COMPLETE ===");
}

fn test_simple_variable() {
    println!("TEST 1: Simple variable substitution (depth 0)");
    println!("Pattern: (_ x)");
    println!("Template: x");
    println!("Input: (test 42)");

    let macro_def = r#"
(define-syntax test
  (syntax-rules ()
    ((_ x) x)))
"#;

    let input = "(test 42)";

    match run_expansion_test(macro_def, input) {
        Ok(result) => {
            println!("✓ Expansion SUCCESS");
            println!("  Result: {}", format_value(&result));
            // Should expand to just: 42
            if let Value::Integer(42) = result {
                println!("  ✓ Correct: expanded to 42");
            } else {
                println!("  ✗ ERROR: expected 42, got {}", format_value(&result));
            }
        }
        Err(e) => println!("✗ Expansion FAILED: {}", e),
    }
    println!();
}

fn test_single_ellipsis() {
    println!("TEST 2: Single ellipsis expansion (depth 1)");
    println!("Pattern: (_ x ...)");
    println!("Template: (list x ...)");
    println!("Input: (test 1 2 3)");

    let macro_def = r#"
(define-syntax test
  (syntax-rules ()
    ((_ x ...) (list x ...))))
"#;

    let input = "(test 1 2 3)";

    match run_expansion_test(macro_def, input) {
        Ok(result) => {
            println!("✓ Expansion SUCCESS");
            println!("  Result: {}", format_value(&result));
            // Should expand to: (list 1 2 3)
            if let Value::List(items) = &result {
                if items.len() == 4 {
                    if let Value::Symbol(s) = &items[0] {
                        if s == "list" {
                            println!("  ✓ Correct: (list 1 2 3)");
                        }
                    }
                } else {
                    println!("  ✗ ERROR: expected 4 elements, got {}", items.len());
                }
            }
        }
        Err(e) => println!("✗ Expansion FAILED: {}", e),
    }
    println!();
}

fn test_nested_ellipsis() {
    println!("TEST 3: Nested ellipsis expansion (depth 2)");
    println!("Pattern: (_ ((x ...) ...))");
    println!("Template: (quote ((x ...) ...))");
    println!("Input: (test ((1 2) (3 4 5)))");

    let macro_def = r#"
(define-syntax test
  (syntax-rules ()
    ((_ ((x ...) ...)) (quote ((x ...) ...)))))
"#;

    let input = "(test ((1 2) (3 4 5)))";

    match run_expansion_test(macro_def, input) {
        Ok(result) => {
            println!("✓ Expansion SUCCESS");
            println!("  Result: {}", format_value(&result));
            // Should expand to: (quote ((1 2) (3 4 5)))
            if let Value::List(items) = &result {
                if items.len() == 2 {
                    if let Value::Symbol(s) = &items[0] {
                        if s == "quote" {
                            println!("  ✓ Correct: nested structure preserved");
                        }
                    }
                }
            }
        }
        Err(e) => println!("✗ Expansion FAILED: {}", e),
    }
    println!();
}

fn test_empty_ellipsis() {
    println!("TEST 4: Empty ellipsis");
    println!("Pattern: (_ x ...)");
    println!("Template: (quote (x ...))");
    println!("Input: (test)");

    let macro_def = r#"
(define-syntax test
  (syntax-rules ()
    ((_ x ...) (quote (x ...)))))
"#;

    let input = "(test)";

    match run_expansion_test(macro_def, input) {
        Ok(result) => {
            println!("✓ Expansion SUCCESS");
            println!("  Result: {}", format_value(&result));
            // Should expand to: (quote ())
            if let Value::List(items) = &result {
                if items.len() == 2 {
                    if let Value::Symbol(s) = &items[0] {
                        if s == "quote" {
                            if let Value::List(inner) = &items[1] {
                                if inner.is_empty() {
                                    println!("  ✓ Correct: empty list produced");
                                }
                            }
                        }
                    }
                }
            }
        }
        Err(e) => println!("✗ Expansion FAILED: {}", e),
    }
    println!();
}

fn test_multiple_vars_ellipsis() {
    println!("TEST 5: Multiple variables in ellipsis");
    println!("Pattern: (_ (name val) ...)");
    println!("Template: (quote ((name val) ...))");
    println!("Input: (test (x 1) (y 2) (z 3))");

    let macro_def = r#"
(define-syntax test
  (syntax-rules ()
    ((_ (name val) ...) (quote ((name val) ...)))))
"#;

    let input = "(test (x 1) (y 2) (z 3))";

    match run_expansion_test(macro_def, input) {
        Ok(result) => {
            println!("✓ Expansion SUCCESS");
            println!("  Result: {}", format_value(&result));
            // Should expand to: (quote ((x 1) (y 2) (z 3)))
            if let Value::List(items) = &result {
                if items.len() == 2 {
                    if let Value::Symbol(s) = &items[0] {
                        if s == "quote" {
                            if let Value::List(inner) = &items[1] {
                                if inner.len() == 3 {
                                    println!("  ✓ Correct: 3 pairs produced");
                                }
                            }
                        }
                    }
                }
            }
        }
        Err(e) => println!("✗ Expansion FAILED: {}", e),
    }
    println!();
}

fn run_expansion_test(macro_def: &str, input: &str) -> Result<Value, MacroError> {
    // Parse macro definition
    let parsed =
        parse_multiple(macro_def).map_err(|e| MacroError(format!("Parse error: {:?}", e)))?;

    let macro_value = parsed
        .first()
        .ok_or_else(|| MacroError("No macro definition found".to_string()))?;

    // Extract macro name and syntax-rules form
    let (name, syntax_rules) = if let Value::List(items) = &macro_value {
        if items.len() >= 3 {
            if let (Value::Symbol(def), Value::Symbol(name), syntax_rules) =
                (&items[0], &items[1], &items[2])
            {
                if def == "define-syntax" {
                    (name.as_str(), syntax_rules)
                } else {
                    return Err(MacroError("Expected define-syntax".to_string()));
                }
            } else {
                return Err(MacroError("Invalid macro definition format".to_string()));
            }
        } else {
            return Err(MacroError("Invalid macro definition".to_string()));
        }
    } else {
        return Err(MacroError("Macro definition must be a list".to_string()));
    };

    // Compile macro
    let compiled = parse_syntax_rules(name, syntax_rules)?;

    // Parse input
    let input_value =
        parse(input).map_err(|e| MacroError(format!("Parse input error: {:?}", e)))?;

    // Extract the arguments (entire input list for pattern matching)
    let args = if let Value::List(_) = &input_value {
        input_value
    } else {
        return Err(MacroError("Input must be a list".to_string()));
    };

    // Try each rule until one matches
    for (i, rule) in compiled.compiled_rules.iter().enumerate() {
        println!("  Trying rule {}/{}", i + 1, compiled.compiled_rules.len());

        // Match pattern
        let match_result = match_pattern(&rule.pattern, &args, &compiled.literals);

        match match_result {
            MatchResult::Success(context) => {
                println!("  Pattern matched!");

                // Expand template with pattern variables from the compiled pattern
                let expanded = expand_template(&rule.template, &context, &rule.pattern.variables)?;
                return Ok(expanded);
            }
            MatchResult::Failure(reason) => {
                println!("  Rule {} did not match: {}", i + 1, reason);
                continue;
            }
        }
    }

    Err(MacroError("No rule matched".to_string()))
}

fn format_value(v: &Value) -> String {
    match v {
        Value::Symbol(s) => s.clone(),
        Value::Integer(n) => n.to_string(),
        Value::Boolean(b) => if *b { "#t" } else { "#f" }.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::List(items) => {
            let strs: Vec<_> = items.iter().map(format_value).collect();
            format!("({})", strs.join(" "))
        }
        _ => format!("{:?}", v),
    }
}
