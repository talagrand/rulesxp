use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::parser::parse_multiple;
use samplescheme::vm::VM;

fn main() {
    println!("=== SYSTEMATIC CPS MACRO DEBUGGING ===\n");

    // Step 1: Test parsing individual simple macros
    println!("Step 1: Testing simple macro parsing...");

    let simple_macro = "(define-syntax simple-test (syntax-rules () ((simple-test x) x)))";
    match parse(simple_macro) {
        Ok(ast) => println!("✓ Simple macro parses: {}", ast),
        Err(e) => {
            println!("✗ Simple macro parse failed: {}", e);
            return;
        }
    }

    // Step 2: Test the current CPS macros file directly
    println!("\nStep 2: Testing current CPS macros file...");
    const CPS_MACROS: &str = include_str!("../../prelude/cps_macros.scm");

    println!("CPS macros file content:");
    println!("--- BEGIN ---");
    println!("{}", CPS_MACROS);
    println!("--- END ---");

    match parse_multiple(CPS_MACROS) {
        Ok(expressions) => {
            println!(
                "✓ CPS macros file parsed successfully ({} expressions)",
                expressions.len()
            );
            for (i, expr) in expressions.iter().enumerate() {
                println!("  Expression {}: {}", i + 1, expr);
            }
        }
        Err(e) => {
            println!("✗ CPS macros file parse failed: {}", e);

            // Step 3: Try parsing line by line to isolate the issue
            println!("\nStep 3: Parsing line by line to isolate issue...");
            let lines: Vec<&str> = CPS_MACROS.lines().collect();
            let mut current_expr = String::new();
            let mut paren_count = 0;

            for (line_num, line) in lines.iter().enumerate() {
                let trimmed = line.trim();
                if trimmed.is_empty() || trimmed.starts_with(";;") {
                    continue;
                }

                current_expr.push_str(line);
                current_expr.push('\n');

                // Count parentheses to detect complete expressions
                for ch in line.chars() {
                    match ch {
                        '(' => paren_count += 1,
                        ')' => paren_count -= 1,
                        _ => {}
                    }
                }

                // When we have a complete expression, try parsing it
                if paren_count == 0 && !current_expr.trim().is_empty() {
                    println!(
                        "\nTrying to parse expression ending at line {}:",
                        line_num + 1
                    );
                    println!("Expression: {}", current_expr.trim());

                    match parse(&current_expr) {
                        Ok(ast) => println!("✓ Parsed successfully: {}", ast),
                        Err(e) => {
                            println!("✗ Parse failed: {}", e);

                            // Step 4: Try even simpler versions
                            println!("\nStep 4: Trying simplified version...");
                            let simplified = current_expr
                                .lines()
                                .filter(|line| !line.trim().starts_with(";;"))
                                .collect::<Vec<_>>()
                                .join(" ");

                            match parse(&simplified) {
                                Ok(ast) => println!("✓ Simplified version works: {}", ast),
                                Err(e2) => println!("✗ Even simplified version fails: {}", e2),
                            }

                            return;
                        }
                    }

                    current_expr.clear();
                }
            }
        }
    }

    // Step 5: Test macro loading
    println!("\nStep 5: Testing macro loading into expander...");
    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    match macro_expander.load_prelude() {
        Ok(_) => println!("✓ Prelude loaded successfully"),
        Err(e) => println!("✗ Prelude loading failed: {}", e),
    }
}
