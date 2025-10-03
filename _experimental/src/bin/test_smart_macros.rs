// Test the smart macro expansion system
use samplescheme::*;

fn parse_and_expand(expander: &mut MacroExpander, input: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let ast = parse(input)?;
    let expanded = expander.expand(&ast)?;
    Ok(expanded)
}

fn main() {
    println!("Testing smart macro expansion...");
    
    let vm = VM::new();
    let mut expander = MacroExpander::new(vm.current_env());
    
    // Test 1: Define a simple macro that should not need extra passes
    println!("\n1. Defining simple macro...");
    let when_macro = "(define-syntax when
        (syntax-rules ()
            ((when test body) (if test body))))";
    
    match parse_and_expand(&mut expander, when_macro) {
        Ok(result) => println!("   Expanded: {:?}", result),
        Err(e) => println!("   Error: {:?}", e)
    }
    
    // Test 2: Use the simple macro (should not need extra passes)
    println!("\n2. Using simple macro (should not need extra passes)...");
    println!("   Known macros before: {:?}", expander.known_macro_count());
    println!("   Emitted symbols before: {:?}", expander.emitted_symbol_count());
    
    let when_usage = "(when #t \"works\")";
    
    match parse_and_expand(&mut expander, when_usage) {
        Ok(result) => {
            println!("   Expanded: {:?}", result);
            println!("   Known macros after: {:?}", expander.known_macro_count());
            println!("   Emitted symbols after: {:?}", expander.emitted_symbol_count());
            println!("   Emitted symbols: {:?}", expander.emitted_symbols());
        },
        Err(e) => println!("   Error: {:?}", e)
    }
    
    // Test 3: Define a macro that generates another macro usage
    println!("\n3. Defining macro that generates macro usage...");
    let define_simple_macro = "(define-syntax define-simple-macro
        (syntax-rules ()
            ((define-simple-macro name replacement)
             (define-syntax name (syntax-rules () ((name) replacement))))))";
    
    match parse_and_expand(&mut expander, define_simple_macro) {
        Ok(result) => println!("   Expanded: {:?}", result),
        Err(e) => println!("   Error: {:?}", e)
    }
    
    // Test 4: Use the macro that generates another macro usage (should need extra passes)
    println!("\n4. Using macro that generates another macro usage (should need extra passes)...");
    println!("   Known macros before: {:?}", expander.known_macro_count());
    println!("   Emitted symbols before: {:?}", expander.emitted_symbol_count());
    
    let complex_usage = "(define-simple-macro hello \"Hello World!\")";
    
    match parse_and_expand(&mut expander, complex_usage) {
        Ok(result) => {
            println!("   Expanded: {:?}", result);
            println!("   Known macros after: {:?}", expander.known_macro_count());
            println!("   Emitted symbols after: {:?}", expander.emitted_symbol_count());
            println!("   Emitted symbols: {:?}", expander.emitted_symbols());
        },
        Err(e) => println!("   Error: {:?}", e)
    }
}