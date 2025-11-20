use samplescheme::macros::MacroExpander;
use samplescheme::parser::parse_multiple;
use samplescheme::value::{Environment, Value};
use std::env;
use std::fs;
use std::rc::Rc;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.scm>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let content = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    println!("=== MACRO DEBUG TOOL ===");
    println!("File: {}", filename);
    println!();

    // Parse the source
    let expressions = match parse_multiple(&content) {
        Ok(exprs) => exprs,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            std::process::exit(1);
        }
    };

    println!("=== PARSED {} EXPRESSIONS ===", expressions.len());
    for (i, expr) in expressions.iter().enumerate() {
        println!("{}. {}", i + 1, format_value_compact(expr, 100));
    }
    println!();

    // Process macro definitions and expansions
    let env = Rc::new(Environment::new());
    let mut macro_expander = MacroExpander::new(env);
    let mut macro_count = 0;
    let mut non_macro_exprs = Vec::new();

    for expr in expressions {
        if let Value::List(items) = &expr {
            if !items.is_empty() {
                if let Value::Symbol(s) = &items[0] {
                    if s == "define-syntax" {
                        // Just expand to register it - the expander handles definitions
                        match macro_expander.expand(&expr) {
                            Ok(_) => {
                                if let Value::List(def_items) = &expr {
                                    if def_items.len() >= 2 {
                                        if let Value::Symbol(name) = &def_items[1] {
                                            println!("✓ Compiled macro: {}", name);

                                            // Dump the compiled definition
                                            if let Some(macro_def) =
                                                macro_expander.get_macro_definition(name)
                                            {
                                                dump_macro_definition(macro_def);
                                            }

                                            println!();
                                            macro_count += 1;
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!("✗ Macro definition error: {:?}", e);
                                std::process::exit(1);
                            }
                        }
                        continue;
                    }
                }
            }
        }
        non_macro_exprs.push(expr);
    }

    println!("=== MACRO EXPANSION ===");
    println!(
        "Expanding {} non-macro expressions...",
        non_macro_exprs.len()
    );
    println!();

    for (i, expr) in non_macro_exprs.iter().enumerate() {
        println!("Expression {}:", i + 1);
        println!("  Input:  {}", format_value_compact(expr, 80));

        match macro_expander.expand(expr) {
            Ok(expanded) => {
                if !values_equal(&expanded, expr) {
                    println!("  Output: {}", format_value_compact(&expanded, 80));
                    println!("  Status: ✓ EXPANDED");
                } else {
                    println!("  Output: (unchanged)");
                    println!("  Status: - no expansion");
                }
            }
            Err(e) => {
                println!("  Status: ✗ ERROR");
                println!("  Error:  {:?}", e);
            }
        }
        println!();
    }

    println!("=== SUMMARY ===");
    println!("Macros defined: {}", macro_count);
    println!("Expressions processed: {}", non_macro_exprs.len());
}

fn format_value_compact(val: &Value, max_len: usize) -> String {
    let full = format!("{}", val);
    if full.len() <= max_len {
        full
    } else {
        format!("{}...", &full[..max_len])
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

fn dump_macro_definition(macro_def: &samplescheme::macros::MacroDefinition) {
    println!("  Literals: {:?}", macro_def.literals);
    println!("  Rules: {} total", macro_def.rules.len());

    for (i, compiled_rule) in macro_def.compiled_rules.iter().enumerate() {
        println!();
        println!("  Rule {}:", i + 1);
        println!("    Pattern:");
        println!(
            "      Variables: {:?}",
            compiled_rule.pattern.variables.iter().collect::<Vec<_>>()
        );
        println!(
            "      Max depth: {}",
            compiled_rule.pattern.max_ellipsis_depth
        );

        // Show depth for each pattern variable
        let mut depth_info: Vec<_> = compiled_rule.pattern.variable_depths.iter().collect();
        depth_info.sort_by_key(|(name, _)| name.as_str());
        for (var, depth) in depth_info {
            println!("        {} at depth {}", var, depth);
        }

        println!("    Template:");
        println!(
            "      Identifiers: {:?}",
            compiled_rule.template.variables.iter().collect::<Vec<_>>()
        );
        println!(
            "      Max depth: {}",
            compiled_rule.template.max_ellipsis_depth
        );

        // Show depth for each pattern variable (only pattern variables are tracked)
        let mut depth_info: Vec<_> = compiled_rule
            .template
            .pattern_variable_depths
            .iter()
            .collect();
        depth_info.sort_by_key(|(name, _)| name.as_str());
        for (var, depth) in depth_info {
            println!("        {} at depth {}", var, depth);
        }
    }
}
