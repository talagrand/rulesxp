// REPL module - Interactive Read-Eval-Print Loop with rustyline
use crate::cps::CPSTransformer;
use crate::macros::MacroExpander;
use crate::{compile, parse, VM};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    println!("SampleScheme v0.1.0 - A minimal R7RS Scheme interpreter");
    println!("CPS Mode: EXPERIMENTAL (Phase 2)");
    println!("Type (exit) or Ctrl+C to quit");
    println!();

    let mut vm = VM::new();
    let mut rl = DefaultEditor::new()?;
    let mut macro_expander = MacroExpander::new(vm.current_env());
    let mut cps_transformer = CPSTransformer::new();

    // Load R7RS standard macro prelude (derived expressions)
    // This loads all forms listed in macros::STANDARD_DERIVED_EXPRESSIONS
    // from prelude/macros.scm - these are required for R7RS compliance
    if let Err(e) = macro_expander.load_prelude() {
        eprintln!("Error: Failed to load standard macro prelude: {}", e);
        eprintln!("The interpreter cannot function without these essential macros.");
        return Err(Box::new(std::io::Error::other(e.to_string())));
    }

    // Load history if it exists
    let _ = rl.load_history("scheme_history.txt");

    loop {
        let readline = rl.readline("scheme> ");
        match readline {
            Ok(line) => {
                let line = line.trim();

                // Handle special commands
                if line.is_empty() {
                    continue;
                }

                if line == "(exit)" || line == "exit" {
                    println!("Goodbye!");
                    break;
                }

                if line == "(cps-on)" {
                    vm.enable_cps_mode();
                    println!("CPS mode enabled (experimental)");
                    continue;
                }

                if line == "(cps-off)" {
                    vm.disable_cps_mode();
                    println!("CPS mode disabled");
                    continue;
                }

                // Add to history
                rl.add_history_entry(line)?;

                // Parse, expand macros, and evaluate
                match parse(line) {
                    Ok(ast) => {
                        // Expand macros
                        match macro_expander.expand(&ast) {
                            Ok(expanded_ast) => {
                                // Compile with or without CPS transformation
                                let compilation_result = if vm.is_cps_mode() {
                                    // Transform to CPS
                                    let cps_ast = cps_transformer.transform_program(&expanded_ast);
                                    compile(&cps_ast, line.to_string(), vm.current_env())
                                } else {
                                    // Standard compilation (no CPS)
                                    compile(&expanded_ast, line.to_string(), vm.current_env())
                                };

                                match compilation_result {
                                    Ok(module) => {
                                        match vm.execute(&module) {
                                            Ok(result) => {
                                                // Don't print unspecified values (from define, etc.)
                                                if result.type_name() != "unspecified" {
                                                    println!("{}", result);
                                                }
                                            }
                                            Err(e) => {
                                                eprintln!("{}", e);
                                                // Show current stack state for debugging
                                                if !vm.stack_is_empty() {
                                                    eprintln!("\nCurrent stack contents:");
                                                    vm.print_stack();
                                                }
                                            }
                                        }
                                    }
                                    Err(e) => eprintln!("Compile error: {}", e),
                                }
                            }
                            Err(e) => eprintln!("Macro expansion error: {}", e),
                        }
                    }
                    Err(e) => eprintln!("Parse error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    let _ = rl.save_history("scheme_history.txt");

    Ok(())
}
