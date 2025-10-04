use samplescheme::{
    compiler,
    macros::MacroExpander,
    parser,
    value::{Environment, Value},
    vm::VM,
};
use std::rc::Rc;

fn main() {
    println!("Testing New CPS Builtin Architecture");
    println!("===================================");

    // Test simple arithmetic with new CPS builtin approach
    let test_code = "(+ 1 2)";

    println!("Testing: {}", test_code);

    match parser::parse(test_code) {
        Ok(ast) => {
            println!("✓ Parsed AST: {:?}", ast);

            // Create environment with same setup as benchmark
            let env = {
                let global_env = Environment::new();
                let builtins = samplescheme::builtins::create_builtins();
                for (name, value) in builtins {
                    global_env.define(name, value);
                }
                let cps_builtins = samplescheme::cps_builtins::get_cps_builtins();
                for (name, func) in cps_builtins {
                    let cps_builtin = Value::Builtin {
                        name: name.to_string(),
                        arity: samplescheme::value::Arity::AtLeast(0),
                        func,
                    };
                    global_env.define(name.to_string(), cps_builtin);
                }
                Rc::new(global_env)
            };

            // Expand CPS macros
            let mut cps_macro_expander = MacroExpander::new(env.clone());
            cps_macro_expander
                .load_cps_prelude()
                .expect("Failed to load CPS prelude");
            let expanded = cps_macro_expander
                .expand(&ast)
                .expect("CPS macro expansion failed");

            println!("✓ CPS macro expanded: {:?}", expanded);

            // Compile with CPS
            match compiler::compile_with_cps(&expanded, test_code.to_string(), env.clone()) {
                Ok(bytecode) => {
                    println!("✓ CPS compilation successful");

                    // Execute
                    let mut vm = VM::new_with_cps(true);
                    match vm.execute(&bytecode) {
                        Ok(result) => {
                            println!("✅ SUCCESS: CPS execution result: {:?}", result);
                            println!("\nThe new CPS builtin architecture works!");
                        }
                        Err(e) => println!("✗ CPS execution failed: {}", e),
                    }
                }
                Err(e) => println!("✗ CPS compilation failed: {}", e),
            }
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }
}
