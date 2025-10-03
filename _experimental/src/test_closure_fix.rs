#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_closure_creation_fix() {
        let mut vm = VM::new();
        
        // Define make-adder function
        let source = "(define make-adder (lambda (n) (lambda (x) (+ x n))))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env().clone()).unwrap();
        let result = vm.execute(&module);
        assert!(result.is_ok());
        println!("Define make-adder: {:?}", result.unwrap());
        
        // Call make-adder to create add-5 function  
        let source = "(define add-5 (make-adder 5))";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env().clone()).unwrap();
        let result = vm.execute(&module);
        assert!(result.is_ok());
        println!("Create add-5: {:?}", result.unwrap());
        
        // Test that add-5 works correctly
        let source = "(add-5 3)";
        let ast = parse(source).unwrap();
        let module = compile(&ast, source.to_string(), vm.current_env().clone()).unwrap();
        let result = vm.execute(&module);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Integer(8) => println!("SUCCESS: add-5 3 = 8"),
            other => panic!("Expected integer 8, got: {:?}", other),
        }
    }
}