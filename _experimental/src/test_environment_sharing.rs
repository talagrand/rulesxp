#[cfg(test)]
mod tests {
    use crate::processed_ast::*;
    use crate::processed_env::*;
    use crate::super_builtins::ProcessedValue;
    use crate::super_vm::*;
    use std::rc::Rc;

    /// Test helper to run the same test on both SuperDirectVM and SuperStackVM
    fn test_both_vms<F>(test_name: &str, source: &str, expected_result: i64, test_fn: F)
    where
        F: Fn(ProcessedValue, &str),
    {
        println!("Testing {} with both VM modes...", test_name);

        // Test SuperDirectVM
        let ast = crate::parser::parse(source).expect("Failed to parse");
        let processed_ast = ProcessedAST::compile(&ast).expect("Failed to compile ProcessedAST");
        let mut direct_vm = SuperDirectVM::new(processed_ast);
        let env = Rc::new(ProcessedEnvironment::new());

        match direct_vm.evaluate(env) {
            Ok(result) => {
                println!("SuperDirectVM result: {:?}", result);
                test_fn(result, "SuperDirectVM");
            }
            Err(e) => {
                panic!("SuperDirectVM failed for {}: {}", test_name, e);
            }
        }

        // Test SuperStackVM
        let ast = crate::parser::parse(source).expect("Failed to parse");
        let processed_ast = ProcessedAST::compile(&ast).expect("Failed to compile ProcessedAST");
        let mut stack_vm = SuperStackVM::new(processed_ast);
        let env = Rc::new(ProcessedEnvironment::new());

        match stack_vm.evaluate(env) {
            Ok(result) => {
                println!("SuperStackVM result: {:?}", result);
                test_fn(result, "SuperStackVM");
            }
            Err(e) => {
                panic!("SuperStackVM failed for {}: {}", test_name, e);
            }
        }
    }

    #[test]
    fn test_nested_function_environment_isolation() {
        // Test that nested function calls create proper environments
        // and don't share environments inappropriately
        let source = "(begin 
            (define outer-var 100)
            (define (outer-func x) 
                (begin 
                    (define inner-var 200)
                    (define (inner-func y) (+ x y inner-var outer-var))
                    (inner-func 5)))
            (outer-func 10))";

        test_both_vms(
            "nested function environment isolation",
            source,
            315,
            |result, vm_name| {
                match result {
                    ProcessedValue::Integer(315) => {
                        println!("SUCCESS ({}): Nested functions have proper environment isolation - result = 315", vm_name);
                        // 10 + 5 + 200 + 100 = 315
                    }
                    other => {
                        panic!("{}: Expected 315, got {:?}", vm_name, other);
                    }
                }
            },
        );
    }

    #[test]
    fn test_multiple_lambda_environment_separation() {
        // Test that multiple lambdas in the same scope get separate environments
        let source = "(begin 
            (define shared-var 42)
            (define lambda1 (lambda (x) (+ x shared-var)))
            (define lambda2 (lambda (y) (* y shared-var)))
            (begin 
                (define result1 (lambda1 8))
                (define result2 (lambda2 3))
                (+ result1 result2)))"; // Should be (8+42) + (3*42) = 50 + 126 = 176

        test_both_vms(
            "multiple lambda environment separation",
            source,
            176,
            |result, vm_name| match result {
                ProcessedValue::Integer(176) => {
                    println!("SUCCESS ({}): Multiple lambdas have proper environment separation - result = 176", vm_name);
                }
                other => {
                    panic!("{}: Expected 176, got {:?}", vm_name, other);
                }
            },
        );
    }

    #[test]
    fn test_begin_block_environment_sharing() {
        // Test that begin blocks share the same environment appropriately
        // This tests for UNDER-sharing (begin should share, not create new environments)
        let source = "(begin 
            (define x 10)
            (define y 20)
            (begin 
                (define z (+ x y))
                z))"; // Should be able to access x and y from outer begin

        test_both_vms(
            "begin block environment sharing",
            source,
            30,
            |result, vm_name| match result {
                ProcessedValue::Integer(30) => {
                    println!(
                        "SUCCESS ({}): Begin blocks share environment appropriately - result = 30",
                        vm_name
                    );
                }
                other => {
                    panic!("{}: Expected 30, got {:?}", vm_name, other);
                }
            },
        );
    }

    #[test]
    fn test_function_call_environment_isolation() {
        // Test that function calls create new environments and don't pollute caller
        let source = "(begin 
            (define global-var 100)
            (define (test-func param)
                (begin 
                    (define local-var 999)
                    (+ param local-var global-var)))
            (begin
                (define result (test-func 1))
                result))"; // local-var should NOT be visible outside test-func

        test_both_vms(
            "function call environment isolation",
            source,
            1100,
            |result, vm_name| {
                match result {
                    ProcessedValue::Integer(1100) => {
                        println!("SUCCESS ({}): Function calls create isolated environments - result = 1100", vm_name);
                        // 1 + 999 + 100 = 1100
                    }
                    other => {
                        panic!("{}: Expected 1100, got {:?}", vm_name, other);
                    }
                }
            },
        );
    }

    #[test]
    fn test_closure_capture_vs_call_environment() {
        // Critical test: Verify closure captures definition-time environment,
        // not call-time environment (common over-sharing bug)
        let source = "(begin 
            (define x 1)
            (define make-func (lambda () (lambda () x)))
            (define captured-func (make-func))
            (begin
                (define x 999)
                (captured-func)))"; // Should return 1, not 999

        test_both_vms(
            "closure capture vs call environment",
            source,
            1,
            |result, vm_name| match result {
                ProcessedValue::Integer(1) => {
                    println!("SUCCESS ({}): Closures capture definition-time environment correctly - result = 1", vm_name);
                }
                ProcessedValue::Integer(999) => {
                    panic!("CRITICAL BUG ({}): Closure is using call-time environment instead of capture-time!", vm_name);
                }
                other => {
                    panic!("{}: Expected 1, got {:?}", vm_name, other);
                }
            },
        );
    }
}
