#[cfg(test)]
mod comprehensive_evaluator_tests {
    use samplescheme::processed_ast::ProcessedAST;
    use samplescheme::processed_env::ProcessedEnvironment;
    use samplescheme::super_builtins::ProcessedValue;
    use samplescheme::super_vm::SuperStackVM;
    use samplescheme::macros::MacroExpander;
    use samplescheme::value::Environment;
    use samplescheme::parser;
    use std::rc::Rc;

    /// Test result variants for comprehensive testing
    #[derive(Debug)]
    enum TestResult {
        Success(ProcessedValue<'static>), // Evaluation should succeed with this value
        Error,                            // Evaluation should fail (any error)
    }
    use TestResult::*;

    /// Create a ProcessedValue from various primitive types
    fn val(value: impl Into<ProcessedValueWrapper>) -> ProcessedValue<'static> {
        value.into().0
    }

    /// Create a success test result 
    fn success(value: impl Into<ProcessedValueWrapper>) -> TestResult {
        Success(val(value))
    }

    /// Wrapper to allow implementing Into for ProcessedValue creation
    struct ProcessedValueWrapper(ProcessedValue<'static>);

    impl From<i64> for ProcessedValueWrapper {
        fn from(n: i64) -> Self {
            ProcessedValueWrapper(ProcessedValue::Integer(n))
        }
    }

    impl From<i32> for ProcessedValueWrapper {
        fn from(n: i32) -> Self {
            ProcessedValueWrapper(ProcessedValue::Integer(n as i64))
        }
    }

    impl From<bool> for ProcessedValueWrapper {
        fn from(b: bool) -> Self {
            ProcessedValueWrapper(ProcessedValue::Boolean(b))
        }
    }

    impl From<&str> for ProcessedValueWrapper {
        fn from(s: &str) -> Self {
            ProcessedValueWrapper(ProcessedValue::OwnedString(s.to_string()))
        }
    }

    impl From<String> for ProcessedValueWrapper {
        fn from(s: String) -> Self {
            ProcessedValueWrapper(ProcessedValue::OwnedString(s))
        }
    }

    /// Helper function to compare ProcessedValues for equality
    fn values_equal(a: &ProcessedValue, b: &ProcessedValue) -> bool {
        match (a, b) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a == b,
            (ProcessedValue::Boolean(a), ProcessedValue::Boolean(b)) => a == b,
            (ProcessedValue::OwnedString(a), ProcessedValue::OwnedString(b)) => a == b,
            (ProcessedValue::String(a), ProcessedValue::String(b)) => a == b,
            (ProcessedValue::OwnedSymbol(a), ProcessedValue::OwnedSymbol(b)) => a == b,
            (ProcessedValue::Symbol(a), ProcessedValue::Symbol(b)) => a == b,
            (ProcessedValue::Unspecified, ProcessedValue::Unspecified) => true,
            // TODO: Add more comparisons for List, Procedure, etc. as needed
            _ => false,
        }
    }

    /// Execute a single test case with detailed error reporting using SuperStackVM
    fn execute_test_case(input: &str, expected: &TestResult, test_id: &str) -> Result<(), String> {
        let ast = match parser::parse(input) {
            Ok(ast) => ast,
            Err(parse_err) => {
                return Err(format!(
                    "{}: unexpected parse error for '{}': {:?}",
                    test_id, input, parse_err
                ));
            }
        };

        // Create macro expander and load standard prelude
        let macro_env = Rc::new(Environment::new());
        let mut macro_expander = MacroExpander::new(macro_env);
        if let Err(e) = macro_expander.load_prelude() {
            return Err(format!(
                "{}: failed to load macro prelude: {:?}",
                test_id, e
            ));
        }

        // Expand macros in the AST
        let expanded_ast = match macro_expander.expand(&ast) {
            Ok(expanded) => expanded,
            Err(macro_err) => {
                return Err(format!(
                    "{}: macro expansion error for '{}': {:?}",
                    test_id, input, macro_err
                ));
            }
        };

        let processed_ast = match ProcessedAST::compile(&expanded_ast) {
            Ok(ast) => ast,
            Err(compile_err) => {
                return Err(format!(
                    "{}: compilation error for '{}': {:?}",
                    test_id, input, compile_err
                ));
            }
        };

        let mut stack_vm = SuperStackVM::new(processed_ast);
        let env = Rc::new(ProcessedEnvironment::new());

        match (stack_vm.evaluate(env), expected) {
            (Ok(actual), Success(expected_val)) => {
                if values_equal(&actual, expected_val) {
                    Ok(())
                } else {
                    Err(format!("{}: expected {:?}, got {:?}", test_id, expected_val, actual))
                }
            },
            (Err(_), Error) => Ok(()),
            (Ok(actual), Error) => {
                Err(format!("{}: expected error, but evaluation succeeded with: {:?}", test_id, actual))
            },
            (Err(e), Success(expected_val)) => {
                Err(format!("{}: expected {:?}, but got error: {:?}", test_id, expected_val, e))
            },
        }
    }

    /// Simplified test runner for isolated test cases
    fn run_comprehensive_tests(test_cases: Vec<(&str, TestResult)>) {
        for (i, (input, expected)) in test_cases.iter().enumerate() {
            let test_id = format!("#{}", i + 1);
            if let Err(e) = execute_test_case(input, expected, &test_id) {
                panic!("{}", e);
            }
        }
    }

    #[test]
    #[allow(clippy::too_many_lines)] // Comprehensive test coverage is intentionally thorough
    fn test_comprehensive_evaluator_migrated_from_scm_files() {
        let test_cases = vec![
            // === BASIC ARITHMETIC ===
            // From simple_test.scm
            ("(+ 1 2)", success(3)),

            // From debug_cps.scm
            ("(+ 2 3)", success(5)),

            // From test_script.scm - multi-argument addition
            ("(+ 100 200 300)", success(600)),

            // From partial_cps_test.scm - nested arithmetic with many operands
            ("(+ (* 2 3) (* 4 5) (* 6 7) (* 8 9))", success(140)), // 6 + 20 + 42 + 72 = 140

            // From test_cps_opcodes.scm - tests that should generate continuation opcodes
            ("(+ 1 (+ 2 3))", success(6)),
            ("((lambda (x) (+ x 10)) (+ 5 5))", success(20)), // (5+5) = 10, then 10+10 = 20
            ("(+ (+ 1 2) (+ 3 (+ 4 5)))", success(15)), // (1+2) + (3+(4+5)) = 3 + (3+9) = 3 + 12 = 15

            // TODO: test_ellipsis.scm contains macros (and, or, when, cond) - cannot migrate as pure expressions
            // TODO: test_let.scm contains let macro - cannot migrate as pure expressions  
            // TODO: test_simple_letrec_function.scm contains letrec macro - cannot migrate as pure expressions

            // === LAMBDA EXPRESSIONS ===
            // From simple_lambda.scm - immediate lambda invocation
            ("((lambda (x) (+ x 1)) 42)", success(43)),

            // === SINGLE EXPRESSION TESTS ===
            // Pure expression tests (not matching specific deleted .scm files)

            // === CONDITIONAL TESTS ===
            // From simple debug scenarios
            ("(if #t 42 0)", success(42)),
            ("(if #f 42 100)", success(100)),

            // === NESTED OPERATIONS ===
            // Pure expression equivalents (not from specific .scm files)
            ("(+ (+ 1 2) (+ 3 4))", success(10)),
            ("(+ 1 (* 2 3))", success(7)),
            ("(* (+ 1 2) 4)", success(12)),

            // === PARAMETER HANDLING ===
            // Lambda parameter handling tests
            ("((lambda (a b) (+ a b)) 10 20)", success(30)),

            // === MACRO TESTS ===
            // Testing standard R7RS macros from prelude/macros.scm
            
            // and macro tests
            ("(and)", success(true)),
            ("(and #t)", success(true)),
            ("(and #f)", success(false)),
            ("(and #t #t)", success(true)),
            ("(and #t #f)", success(false)),
            ("(and #f #t)", success(false)),
            ("(and 1 2 3)", success(3)), // last value if all truthy
            
            // or macro tests  
            ("(or)", success(false)),
            ("(or #f)", success(false)),
            ("(or #t)", success(true)),
            ("(or #f #t)", success(true)),
            ("(or #t #f)", success(true)),
            ("(or #f #f)", success(false)),
            ("(or #f 42)", success(42)), // first truthy value
            
            // let macro tests
            ("(let ((x 5)) x)", success(5)),
            ("(let ((x 5) (y 10)) (+ x y))", success(15)),
            ("(let ((x 1)) (let ((x 2)) x))", success(2)), // inner binding shadows outer
            
            // === MIGRATED FROM .SCM FILES ===
            // From test_let.scm - let macro with arithmetic
            ("(let ((x 10) (y 20)) (+ x y))", success(30)),
            
            // From test_simple_letrec_function.scm - letrec with lambda
            ("(letrec ((f (lambda (x) (+ x 1)))) (f 41))", success(42)),

            // === ERROR CASES ===
            // From failing_test.scm - this should fail
            ("(undefined-function 1 2)", Error),

            // === BOOLEAN TESTS ===
            ("#t", success(true)),
            ("#f", success(false)),
            ("(not #t)", success(false)),
            ("(not #f)", success(true)),

            // === COMPARISON TESTS ===
            ("(= 5 5)", success(true)),
            ("(= 5 6)", success(false)),
            ("(< 3 5)", success(true)),
            ("(> 5 3)", success(true)),

            // TODO: Add more complex tests that require environment state
            // These will need a different approach for handling defines
        ];

        run_comprehensive_tests(test_cases);
    }
}