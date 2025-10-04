use samplescheme::{
    compiler, parser,
    value::{Environment, Value},
    vm::VM,
};
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.scm>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let source = std::fs::read_to_string(filename)?;

    println!("=== SIMPLE CPS vs NON-CPS TEST ===");
    println!("Source: {}", source.trim());

    // Parse once
    let ast = parser::parse_multiple(&source)?;
    println!("Parsed AST: {:?}", ast);

    // Create environment
    let env = std::rc::Rc::new(Environment::new());

    // Test Non-CPS
    println!("\n=== NON-CPS EVALUATION ===");
    let program = if ast.len() == 1 {
        ast[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(ast.clone());
        Value::List(begin_list)
    };
    let non_cps_module = compiler::compile(&program, source.clone(), env.clone())?;
    let mut non_cps_vm = VM::new_with_env(env.clone(), false);
    let non_cps_result = non_cps_vm.execute(&non_cps_module)?;
    println!("Non-CPS result: {:?}", non_cps_result);

    // Test CPS
    println!("\n=== CPS EVALUATION ===");
    let cps_program = if ast.len() == 1 {
        ast[0].clone()
    } else {
        let mut begin_list = vec![Value::Symbol("begin".to_string())];
        begin_list.extend(ast.clone());
        Value::List(begin_list)
    };
    let cps_module = compiler::compile_with_cps(&cps_program, source.clone(), env.clone())?;
    let mut cps_vm = VM::new_with_env(env.clone(), true);
    let cps_result = cps_vm.execute(&cps_module)?;
    println!("CPS result: {:?}", cps_result);

    // Compare
    println!("\n=== COMPARISON ===");
    if non_cps_result == cps_result {
        println!("✓ Results match: {:?}", non_cps_result);
    } else {
        println!("✗ Results differ:");
        println!("  Non-CPS: {:?}", non_cps_result);
        println!("  CPS:     {:?}", cps_result);
    }

    Ok(())
}
