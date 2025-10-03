// REPL module - Interactive Read-Eval-Print Loop with rustyline
use crate::macros::MacroExpander;
use crate::{compile, parse, VM};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    println!("SampleScheme v0.1.0 - A minimal R7RS Scheme interpreter");
    println!("Type (exit) or Ctrl+C to quit");
    println!();

    let mut vm = VM::new();
    let mut rl = DefaultEditor::new()?;
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load prelude macros
    if let Err(e) = macro_expander.load_prelude() {
        eprintln!("Warning: Failed to load prelude macros: {}", e);
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

                // Add to history
                rl.add_history_entry(line)?;

                // Parse, expand macros, and evaluate
                match parse(line) {
                    Ok(ast) => {
                        // Expand macros
                        match macro_expander.expand(&ast) {
                            Ok(expanded_ast) => {
                                match compile(&expanded_ast, line.to_string(), vm.current_env()) {
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
