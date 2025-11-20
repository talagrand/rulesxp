// Direct matcher test - bypasses the old macros.rs expansion
//
// This tests macro_matcher.rs in isolation to validate the binding construction

use samplescheme::macro_compiler::parse_syntax_rules;
use samplescheme::macro_matcher::{Binding, MatchResult, match_pattern};
use samplescheme::parser::parse;
use samplescheme::value::Value;

fn main() {
    println!("=== MACRO MATCHER DIRECT TEST ===\n");

    // Test 1: Simple variable (depth 0)
    test_simple_variable();

    // Test 2: Single ellipsis (depth 1)
    test_single_ellipsis();

    // Test 3: Nested ellipsis (depth 2)
    test_nested_ellipsis();

    // Test 4: Empty ellipsis
    test_empty_ellipsis();

    // Test 5: Multiple variables in ellipsis
    test_multiple_vars_ellipsis();

    println!("\n=== ALL TESTS COMPLETE ===");
}

fn test_simple_variable() {
    println!("TEST 1: Simple variable (depth 0)");
    println!("Pattern: (_ x)");
    println!("Input: (test 42)");

    let syntax_rules = parse("(syntax-rules () ((_ x) x))").unwrap();
    let macro_def = parse_syntax_rules("test", &syntax_rules).unwrap();
    let compiled = &macro_def.compiled_rules[0];

    let input = parse("(test 42)").unwrap();

    match match_pattern(&compiled.pattern, &input, &macro_def.literals) {
        MatchResult::Success(ctx) => {
            println!("✓ Match SUCCESS");
            for (var, binding) in &ctx.bindings {
                println!("  {} = {}", var, binding.debug_format());

                // Validate: x should be Direct(42)
                if var == "x" {
                    if let Binding::Direct(Value::Integer(42)) = binding {
                        println!("  ✓ Correct: x is Direct(42)");
                    } else {
                        println!("  ✗ ERROR: x has wrong binding: {}", binding.debug_format());
                    }
                }
            }
        }
        MatchResult::Failure(msg) => {
            println!("✗ Match FAILED: {}", msg);
        }
    }
    println!();
}

fn test_single_ellipsis() {
    println!("TEST 2: Single ellipsis (depth 1)");
    println!("Pattern: (_ x ...)");
    println!("Input: (test 1 2 3)");

    let syntax_rules = parse("(syntax-rules () ((_ x ...) (quote (x ...))))").unwrap();
    let macro_def = parse_syntax_rules("test", &syntax_rules).unwrap();
    let compiled = &macro_def.compiled_rules[0];

    let input = parse("(test 1 2 3)").unwrap();

    match match_pattern(&compiled.pattern, &input, &macro_def.literals) {
        MatchResult::Success(ctx) => {
            println!("✓ Match SUCCESS");
            for (var, binding) in &ctx.bindings {
                println!("  {} = {}", var, binding.debug_format());

                // Validate: x should be Repeated with count 3
                if var == "x" {
                    if let Binding::Repeated { count, elements } = binding {
                        println!("  ✓ Correct: x is Repeated with count {}", count);
                        if *count != 3 {
                            println!("  ✗ ERROR: Expected count 3, got {}", count);
                        }
                        // Check each element
                        for (i, elem) in elements.iter().enumerate() {
                            if let Binding::Direct(Value::Integer(n)) = elem {
                                if *n as usize != i + 1 {
                                    println!(
                                        "  ✗ ERROR: Element {} should be {}, got {}",
                                        i,
                                        i + 1,
                                        n
                                    );
                                }
                            } else {
                                println!("  ✗ ERROR: Element {} is not Direct(Integer)", i);
                            }
                        }
                    } else {
                        println!(
                            "  ✗ ERROR: x should be Repeated, got {}",
                            binding.debug_format()
                        );
                    }
                }
            }
        }
        MatchResult::Failure(msg) => {
            println!("✗ Match FAILED: {}", msg);
        }
    }
    println!();
}

fn test_nested_ellipsis() {
    println!("TEST 3: Nested ellipsis (depth 2)");
    println!("Pattern: (_ ((x ...) ...))");
    println!("Input: (test ((1 2) (3 4 5)))");

    let syntax_rules =
        parse("(syntax-rules () ((_ ((x ...) ...)) (quote ((x ...) ...))))").unwrap();
    let macro_def = parse_syntax_rules("test", &syntax_rules).unwrap();
    let compiled = &macro_def.compiled_rules[0];

    let input = parse("(test ((1 2) (3 4 5)))").unwrap();

    match match_pattern(&compiled.pattern, &input, &macro_def.literals) {
        MatchResult::Success(ctx) => {
            println!("✓ Match SUCCESS");
            for (var, binding) in &ctx.bindings {
                println!("  {} = {}", var, binding.debug_format());

                // Validate: x should be Repeated of Repeated
                if var == "x" {
                    if let Binding::Repeated {
                        count: outer_count,
                        elements: outer_elements,
                    } = binding
                    {
                        println!(
                            "  ✓ Correct: x is Repeated(outer) with count {}",
                            outer_count
                        );
                        if *outer_count != 2 {
                            println!("  ✗ ERROR: Expected outer count 2, got {}", outer_count);
                        }

                        // Check first inner list
                        if let Some(Binding::Repeated {
                            count: inner_count,
                            elements: inner_elements,
                        }) = outer_elements.first()
                        {
                            println!("    Inner[0]: Repeated with count {}", inner_count);
                            if *inner_count != 2 {
                                println!(
                                    "    ✗ ERROR: Expected inner[0] count 2, got {}",
                                    inner_count
                                );
                            }
                        } else {
                            println!("    ✗ ERROR: Inner[0] should be Repeated");
                        }

                        // Check second inner list
                        if let Some(Binding::Repeated {
                            count: inner_count, ..
                        }) = outer_elements.get(1)
                        {
                            println!("    Inner[1]: Repeated with count {}", inner_count);
                            if *inner_count != 3 {
                                println!(
                                    "    ✗ ERROR: Expected inner[1] count 3, got {}",
                                    inner_count
                                );
                            }
                        } else {
                            println!("    ✗ ERROR: Inner[1] should be Repeated");
                        }
                    } else {
                        println!(
                            "  ✗ ERROR: x should be Repeated, got {}",
                            binding.debug_format()
                        );
                    }
                }
            }
        }
        MatchResult::Failure(msg) => {
            println!("✗ Match FAILED: {}", msg);
        }
    }
    println!();
}

fn test_empty_ellipsis() {
    println!("TEST 4: Empty ellipsis");
    println!("Pattern: (_ x ...)");
    println!("Input: (test)");

    let syntax_rules = parse("(syntax-rules () ((_ x ...) (quote (x ...))))").unwrap();
    let macro_def = parse_syntax_rules("test", &syntax_rules).unwrap();
    let compiled = &macro_def.compiled_rules[0];

    let input = parse("(test)").unwrap();

    match match_pattern(&compiled.pattern, &input, &macro_def.literals) {
        MatchResult::Success(ctx) => {
            println!("✓ Match SUCCESS");
            for (var, binding) in &ctx.bindings {
                println!("  {} = {}", var, binding.debug_format());

                // Validate: x should be Repeated with count 0
                if var == "x" {
                    if let Binding::Repeated { count, .. } = binding {
                        if *count == 0 {
                            println!("  ✓ Correct: x is Repeated with count 0 (empty)");
                        } else {
                            println!("  ✗ ERROR: Expected count 0, got {}", count);
                        }
                    } else {
                        println!(
                            "  ✗ ERROR: x should be Repeated, got {}",
                            binding.debug_format()
                        );
                    }
                }
            }
        }
        MatchResult::Failure(msg) => {
            println!("✗ Match FAILED: {}", msg);
        }
    }
    println!();
}

fn test_multiple_vars_ellipsis() {
    println!("TEST 5: Multiple variables in ellipsis");
    println!("Pattern: (_ (name val) ...)");
    println!("Input: (test (x 1) (y 2) (z 3))");

    let syntax_rules =
        parse("(syntax-rules () ((_ (name val) ...) (quote ((name val) ...))))").unwrap();
    let macro_def = parse_syntax_rules("test", &syntax_rules).unwrap();
    let compiled = &macro_def.compiled_rules[0];

    let input = parse("(test (x 1) (y 2) (z 3))").unwrap();

    match match_pattern(&compiled.pattern, &input, &macro_def.literals) {
        MatchResult::Success(ctx) => {
            println!("✓ Match SUCCESS");
            for (var, binding) in &ctx.bindings {
                println!("  {} = {}", var, binding.debug_format());

                // Both name and val should be Repeated with count 3
                if let Binding::Repeated { count, elements } = binding {
                    if *count == 3 {
                        println!("  ✓ Correct: {} is Repeated with count 3", var);

                        // Check that each element is Direct
                        for (i, elem) in elements.iter().enumerate() {
                            if !matches!(elem, Binding::Direct(_)) {
                                println!("    ✗ ERROR: Element {} should be Direct", i);
                            }
                        }
                    } else {
                        println!("  ✗ ERROR: Expected count 3, got {}", count);
                    }
                } else {
                    println!("  ✗ ERROR: {} should be Repeated", var);
                }
            }
        }
        MatchResult::Failure(msg) => {
            println!("✗ Match FAILED: {}", msg);
        }
    }
    println!();
}
