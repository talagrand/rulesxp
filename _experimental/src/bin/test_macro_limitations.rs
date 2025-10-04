use samplescheme::macros::MacroExpander;
use samplescheme::parse;
use samplescheme::vm::VM;

fn main() {
    println!("=== TESTING MACRO SYSTEM LIMITATIONS ===\n");

    let vm = VM::new();
    let mut macro_expander = MacroExpander::new(vm.current_env());

    // Load the prelude
    match macro_expander.load_prelude() {
        Ok(_) => println!("✓ Prelude loaded successfully"),
        Err(e) => {
            println!("✗ Prelude loading failed: {}", e);
            return;
        }
    }

    println!("\n=== NESTED ELLIPSIS LIMITATIONS ===");
    
    // Test 1: Standard R7RS `do` macro with nested ellipsis
    println!("Test 1: Full R7RS do macro");
    let full_do_macro = r#"
(define-syntax do-full
  (syntax-rules ()
    ((do-full ((var init step ...) ...) (test expr ...) command ...)
     (let loop ((var init) ...)
       (if test
           (begin expr ...)
           (begin command ... (loop step ... ...)))))))
    "#;
    
    test_macro_definition(&mut macro_expander, full_do_macro, "Full do macro with nested ellipsis");
    
    // Test 2: Multiple binding forms with steps
    println!("\nTest 2: Let with optional step values");
    let let_with_steps = r#"
(define-syntax let-steps
  (syntax-rules ()
    ((let-steps ((var init step ...) ...) body ...)
     (let loop ((var init) ...)
       (begin body ...)))))
    "#;
    
    test_macro_definition(&mut macro_expander, let_with_steps, "Let with optional step values");
    
    // Test 3: Function definition with variable argument lists  
    println!("\nTest 3: Function with variable rest arguments");
    let func_with_rest = r#"
(define-syntax defun-rest
  (syntax-rules ()
    ((defun-rest name ((arg ...) rest-args ...) body ...)
     (define name 
       (lambda (arg ... . rest-args)
         body ...)))))
    "#;
    
    test_macro_definition(&mut macro_expander, func_with_rest, "Function with variable rest arguments");

    println!("\n=== PATTERN COMPLEXITY LIMITATIONS ===");
    
    // Test 4: CPS transformation with complex pattern matching
    println!("Test 4: Advanced CPS with multiple continuation patterns");
    let complex_cps = r#"
(define-syntax cps-multi
  (syntax-rules (lambda if call/cc)
    ; Multiple lambda patterns with different arities
    ((cps-multi (lambda () body) k)
     (k (lambda (cont) (cps-multi body cont))))
    ((cps-multi (lambda (x) body) k) 
     (k (lambda (cont x) (cps-multi body cont))))
    ((cps-multi (lambda (x y) body) k)
     (k (lambda (cont x y) (cps-multi body cont))))
    ; Call/cc pattern
    ((cps-multi (call/cc f) k)
     (f (lambda (escape) (escape k))))
    ; Multiple if patterns with different structures
    ((cps-multi (if test) k)
     (cps-multi test (lambda (t) (if t (k #t) (k #f)))))
    ((cps-multi (if test then) k)
     (cps-multi test (lambda (t) (if t (cps-multi then k) (k #f)))))
    ((cps-multi (if test then else) k)
     (cps-multi test (lambda (t) (if t (cps-multi then k) (cps-multi else k)))))
    ; Catch-all
    ((cps-multi expr k) (k expr))))
    "#;
    
    test_macro_definition(&mut macro_expander, complex_cps, "Complex CPS with multiple patterns");
    
    // Test 5: Syntax with context-dependent patterns
    println!("\nTest 5: Context-dependent macro patterns");
    let context_macro = r#"
(define-syntax with-context
  (syntax-rules (in at from)
    ((with-context (in env) (at location) (from source) body ...)
     (let ((current-env env)
           (current-loc location) 
           (current-src source))
       body ...))
    ((with-context (at location) (from source) body ...)
     (let ((current-loc location)
           (current-src source))  
       body ...))
    ((with-context (from source) body ...)
     (let ((current-src source))
       body ...))))
    "#;
    
    test_macro_definition(&mut macro_expander, context_macro, "Context-dependent patterns");

    println!("\n=== TESTING WHAT ACTUALLY WORKS ===");
    
    // Test 6: Simple patterns that work
    println!("Test 6: Simple macro that should work");
    let simple_macro = r#"
(define-syntax simple-when  
  (syntax-rules ()
    ((simple-when test body ...)
     (if test (begin body ...)))))
    "#;
    
    test_macro_definition(&mut macro_expander, simple_macro, "Simple when macro");
    
    if test_macro_expansion(&mut macro_expander, "(simple-when #t 1 2 3)", "Simple when expansion") {
        println!("✓ Simple macro works correctly");
    }

    println!("\n=== SUMMARY ===");
    println!("Current macro system limitations:");
    println!("❌ Cannot handle nested ellipsis patterns ((a b ...) ...)");
    println!("❌ Complex multi-pattern matching with many alternatives fails");
    println!("❌ Context-dependent literal matching has issues");
    println!("✅ Simple ellipsis patterns work fine");
    println!("✅ Basic pattern matching with literals works");
}

fn test_macro_definition(macro_expander: &mut MacroExpander, macro_def: &str, description: &str) {
    print!("  Testing {}: ", description);
    match parse(macro_def) {
        Ok(ast) => {
            match macro_expander.expand(&ast) {
                Ok(expanded) => {
                    println!("✓ Parsed successfully");
                    println!("    Expansion: {}", expanded);
                    
                    // The expansion of a define-syntax should register the macro
                    // but we can't easily test this without executing it
                }
                Err(e) => println!("❌ Expansion failed: {}", e),
            }
        }
        Err(e) => println!("❌ Parse failed: {}", e),
    }
}

fn test_macro_expansion(macro_expander: &mut MacroExpander, input: &str, description: &str) -> bool {
    print!("  Testing {}: ", description);
    match parse(input) {
        Ok(ast) => {
            match macro_expander.expand(&ast) {
                Ok(expanded) => {
                    println!("✓ {} -> {}", input, expanded);
                    true
                }
                Err(e) => {
                    println!("❌ Expansion failed: {}", e);
                    false
                }
            }
        }
        Err(e) => {
            println!("❌ Parse failed: {}", e);
            false
        }
    }
}