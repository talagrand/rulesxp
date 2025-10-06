#[cfg(test)]
mod tests {
    use crate::processed_ast::*;
    use crate::processed_env::*;
    use crate::super_builtins::ProcessedValue;
    use crate::super_vm::*;
    use std::rc::Rc;

    /// Test helper to run the same recursive function test on both VM modes
    fn test_recursive_both_vms<F>(test_name: &str, source: &str, test_fn: F)
    where
        F: Fn(ProcessedValue, &str) + Copy,
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
    fn test_recursive_factorial_definition_only() {
        // Test that we can at least define a recursive factorial function
        let source = "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))";

        test_recursive_both_vms(
            "recursive factorial definition",
            source,
            |result, vm_name| match result {
                ProcessedValue::Unspecified => {
                    println!(
                        "SUCCESS ({}): Recursive factorial defined successfully",
                        vm_name
                    );
                }
                other => {
                    panic!(
                        "{}: Expected Unspecified from define, got {:?}",
                        vm_name, other
                    );
                }
            },
        );
    }

    #[test]
    fn test_recursive_factorial_with_begin() {
        // Test recursive factorial by combining definition and call in begin block
        let source = "(begin (define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1)))))) (factorial 5))";

        test_recursive_both_vms(
            "recursive factorial with begin",
            source,
            |result, vm_name| match result {
                ProcessedValue::Integer(120) => {
                    println!("SUCCESS ({}): factorial(5) = 120", vm_name);
                }
                other => {
                    panic!("{}: Expected 120, got {:?}", vm_name, other);
                }
            },
        );
    }

    #[test]
    fn test_simple_recursive_countdown() {
        // Test simpler recursive function
        let source = "(begin (define countdown (lambda (n) (if (= n 0) 42 (countdown (- n 1))))) (countdown 3))";

        test_recursive_both_vms(
            "simple recursive countdown",
            source,
            |result, vm_name| match result {
                ProcessedValue::Integer(42) => {
                    println!("SUCCESS ({}): countdown(3) = 42", vm_name);
                }
                other => {
                    panic!("{}: Expected 42, got {:?}", vm_name, other);
                }
            },
        );
    }
}
