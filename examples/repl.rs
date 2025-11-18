use rulesxp::Error;
use rulesxp::ParseErrorKind;
use rulesxp::ast::Value;
use rulesxp::evaluator;
use rulesxp::jsonlogic::{ast_to_jsonlogic, parse_jsonlogic};
use rulesxp::scheme::{ParseConfig, parse_scheme_with_config};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::panic;
use std::process;

/// Check if an error indicates incomplete input that needs continuation
fn is_incomplete_error(error: &Error) -> bool {
    match error {
        Error::ParseError(parse_err) => {
            matches!(parse_err.kind, ParseErrorKind::Incomplete)
                || parse_err.message.contains("Expected closing ')'")
                || parse_err.message.contains("Expected symbol characters")
                || parse_err.message.contains("Expected valid string literal")
        }
        _ => false,
    }
}

fn main() {
    let result = panic::catch_unwind(|| {
        run_repl();
    });

    if let Err(panic_info) = result {
        eprintln!("The REPL encountered an unexpected error and must exit.");

        if let Some(msg) = panic_info.downcast_ref::<&str>() {
            eprintln!("Error: {msg}");
        } else if let Some(msg) = panic_info.downcast_ref::<String>() {
            eprintln!("Error: {msg}");
        } else {
            eprintln!("Error: Unknown panic occurred");
        }

        process::exit(1);
    }
}

fn run_repl() {
    println!("RulesXP Multi-Language Rules Expression Evaluator");
    println!("Supports JSONLogic and Scheme with strict typing");
    println!("Enter S-expressions like: (+ 1 2)");
    println!("Enter JSONLogic like: {{\"and\": [true, {{\">\":[5,3]}}]}}");
    println!("Type :help for more commands, or Ctrl+C to exit.");
    println!();

    let mut rl = DefaultEditor::new().expect("Could not initialize REPL");
    let mut env = evaluator::create_global_env();

    // Register custom function that can be called from user code for demonstration purposes
    env.register_builtin_operation::<()>("help", print_help);

    let mut jsonlogic_mode = false;
    let mut accumulated_input = String::new();

    loop {
        let prompt = if accumulated_input.is_empty() {
            "rulesxp> "
        } else {
            "     ... "
        };

        match rl.readline(prompt) {
            Ok(line) => {
                let line_trimmed = line.trim();

                // Accumulate input
                if !accumulated_input.is_empty() {
                    accumulated_input.push('\n');
                }
                accumulated_input.push_str(&line);

                // Skip empty lines unless we're accumulating
                if line_trimmed.is_empty() && accumulated_input.trim().is_empty() {
                    accumulated_input.clear();
                    continue;
                }

                // Add the accumulated input to history only when complete
                // (we'll add it later when we successfully parse)

                // Add the accumulated input to history only when complete
                // (we'll add it later when we successfully parse)

                // Handle special commands (only when not accumulating multi-line input)
                if accumulated_input.trim().starts_with(':') && !accumulated_input.contains('\n') {
                    match accumulated_input.trim() {
                        ":help" => {
                            _ = print_help().is_ok();
                            accumulated_input.clear();
                            continue;
                        }
                        ":env" => {
                            print_environment(&env);
                            accumulated_input.clear();
                            continue;
                        }
                        ":jsonlogic" => {
                            jsonlogic_mode = !jsonlogic_mode;
                            if jsonlogic_mode {
                                println!("JSONLogic mode enabled:");
                                println!("  • Results shown as JSONLogic");
                                println!("  • Scheme inputs show JSONLogic translation (→)");
                            } else {
                                println!("Scheme mode enabled:");
                                println!("  • Results shown as S-expressions");
                                println!("  • JSONLogic inputs show Scheme translation (→)");
                            }
                            accumulated_input.clear();
                            continue;
                        }
                        ":quit" | ":exit" => {
                            println!("Goodbye!");
                            break;
                        }
                        _ => {}
                    }
                }

                // Try to parse and evaluate the expression
                // First check if input looks like JSON (starts with { or [)
                let input = accumulated_input.trim();
                let result = if input.starts_with('{') || input.starts_with('[') {
                    // Try JSONLogic parsing
                    match parse_jsonlogic(input) {
                        Ok(expr) => {
                            // If in Scheme mode, show the parsed expression as Scheme
                            if !jsonlogic_mode {
                                println!("→ {expr}");
                            }
                            Ok(Some(evaluator::eval(&expr, &mut env)))
                        }
                        Err(e) => {
                            // Check if error is due to incomplete input
                            if is_incomplete_error(&e) {
                                Ok(None) // Signal to continue accumulating
                            } else {
                                Err(e)
                            }
                        }
                    }
                } else {
                    // Try Scheme parsing with script syntax enabled (comments and line continuations)
                    let config = ParseConfig {
                        script_syntax: true,
                    };
                    match parse_scheme_with_config(input, config) {
                        Ok(expr) => {
                            // If in JSONLogic mode, show the parsed expression as JSONLogic
                            if jsonlogic_mode && let Ok(json_str) = ast_to_jsonlogic(&expr) {
                                println!("→ {json_str}");
                            } // Skip if conversion fails
                            Ok(Some(evaluator::eval(&expr, &mut env)))
                        }
                        Err(e) => {
                            // Check if error is due to incomplete input
                            if is_incomplete_error(&e) {
                                Ok(None) // Signal to continue accumulating
                            } else {
                                Err(e)
                            }
                        }
                    }
                };

                match result {
                    Ok(Some(eval_result)) => {
                        // Successfully parsed and evaluated - add to history and clear accumulator
                        let _ = rl.add_history_entry(&accumulated_input);
                        accumulated_input.clear();

                        match eval_result {
                            Ok(result) => {
                                // Don't print Unspecified values (e.g., from define)
                                if !matches!(result, Value::Unspecified) {
                                    if jsonlogic_mode {
                                        match ast_to_jsonlogic(&result) {
                                            Ok(json_str) => println!("{json_str}"),
                                            Err(_) => println!("{result}"), // Fallback to S-expression if conversion fails
                                        }
                                    } else {
                                        println!("{result}");
                                    }
                                }
                            }
                            Err(e) => {
                                println!("Error: {e}");
                            }
                        }
                    }
                    Ok(None) => {
                        // Incomplete input - continue accumulating on next iteration
                        // The loop will show the continuation prompt
                    }
                    Err(e) => {
                        // Real error - show it and clear accumulator
                        println!("Error: {e}");
                        accumulated_input.clear();
                    }
                }
            }

            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => {
                // On Ctrl+C or Ctrl+D, clear accumulated input if any, otherwise exit
                if !accumulated_input.is_empty() {
                    println!("^C");
                    accumulated_input.clear();
                } else {
                    println!("Goodbye!");
                    break;
                }
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }
}

fn print_help() -> Result<Value, Error> {
    println!("Mini Scheme Interpreter with JSONLogic Support:");
    println!("  :help      - Show this help message");
    println!("  :env       - Show current environment bindings");
    println!("  :jsonlogic - Toggle JSONLogic output mode");
    println!(
        "               • Scheme mode: results as S-expressions, shows JSONLogic translation (→)"
    );
    println!("               • JSONLogic mode: results as JSONLogic, shows Scheme translation (→)");
    println!("  :quit      - Exit the interpreter");
    println!("  :exit      - Exit the interpreter");
    println!("  Ctrl+C     - Exit the interpreter");
    println!();
    println!("Supported languages:");
    println!("  S-expressions (Scheme): (+ 1 2), (and #t (> 5 3))");
    println!("  JSONLogic: {{\"and\": [true, {{\">\":[5,3]}}]}}");
    println!();
    println!("Supported operations (both languages):");
    println!("  Numbers: 42, -5");
    println!("  Booleans: #t/#f (Scheme) or true/false (JSON)");
    println!("  Arithmetic: +, -, *, /");
    println!("  Comparison: =, <, >, <=, >=, !=");
    println!("  Logic: and, or, not");
    println!("  Conditionals: if");
    println!("  Variables: var (JSONLogic)");
    println!();
    println!("Examples:");
    println!("  (+ 1 2 3)");
    println!("  {{\">\": [5, 3]}}");
    println!("  (and #t (> 5 3))");
    println!("  {{\"and\": [true, {{\">\":[5,3]}}]}}");
    println!();

    Ok(Value::Unspecified)
}

fn print_environment(env: &rulesxp::evaluator::Environment) {
    let bindings = env.get_all_bindings();

    if bindings.is_empty() {
        println!("Environment is empty.");
        return;
    }

    println!("Environment bindings ({} total):", bindings.len());
    println!();

    // Separate built-in functions from user-defined values
    let mut builtins = Vec::new();
    let mut user_defined = Vec::new();

    for (name, value) in bindings {
        match value {
            rulesxp::ast::Value::BuiltinFunction { .. } => builtins.push(name),
            _ => user_defined.push((name, value)),
        }
    }

    // Print built-in functions
    if !builtins.is_empty() {
        println!("Built-in functions ({}):", builtins.len());
        // Print in columns for readability
        let mut col = 0;
        for name in builtins {
            print!("  {name:<15}");
            col += 1;
            if col % 4 == 0 {
                println!();
            }
        }
        if col % 4 != 0 {
            println!();
        }
        println!();
    }

    // Print user-defined values
    if !user_defined.is_empty() {
        println!("User-defined values ({}):", user_defined.len());
        for (name, value) in user_defined {
            println!("  {name} = {value}");
        }
    }
}
