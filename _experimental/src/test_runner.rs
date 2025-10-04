// Test infrastructure for the Scheme interpreter
use crate::compiler;
use crate::macros::MacroExpander;
use crate::parser;
use crate::value::{Environment, Value};
use crate::vm::VM;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// What aspects of the test execution to verify
#[derive(Debug, Clone, PartialEq)]
pub enum TestMode {
    /// Compare bytecode against .asm file  
    Bytecode,
    /// Compare evaluation results against .result file
    Result,
    /// Run both bytecode and result checks
    All,
}

/// Result of running a single test
#[derive(Debug)]
pub struct TestResult {
    pub test_name: String,
    pub mode: TestMode,
    pub passed: bool,
    pub expected: String,
    pub actual: String,
    pub diff: Option<String>,
}

/// Test runner configuration
pub struct TestRunner {
    test_dir: PathBuf,
    update_references: bool,
}

impl TestRunner {
    /// Create a new test runner
    pub fn new(test_dir: impl Into<PathBuf>) -> Self {
        let update_references = std::env::var("UPDATE_TEST_REFERENCES")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false);

        TestRunner {
            test_dir: test_dir.into(),
            update_references,
        }
    }

    /// Parse and expand macros in source code
    fn parse_and_expand(
        &self,
        source_code: &str,
        vm_env: Rc<Environment>,
    ) -> Result<Vec<Value>, TestError> {
        // Parse the source code first
        let parsed = parser::parse_multiple(source_code)
            .map_err(|e| TestError::ParseError(format!("{:?}", e)))?;

        // Create macro expander and load prelude
        let mut macro_expander = MacroExpander::new(vm_env);
        macro_expander
            .load_prelude()
            .map_err(|e| TestError::MacroError(format!("{}", e)))?;

        // Expand macros for each expression
        let mut expanded = Vec::new();
        for expr in parsed {
            let expanded_expr = macro_expander
                .expand(&expr)
                .map_err(|e| TestError::MacroError(format!("{}", e)))?;
            expanded.push(expanded_expr);
        }

        Ok(expanded)
    }

    /// Run a single test file
    pub fn run_test(&self, test_name: &str, mode: TestMode) -> Result<Vec<TestResult>, TestError> {
        let test_file = self.test_dir.join(format!("{}.scm", test_name));

        if !test_file.exists() {
            return Err(TestError::FileNotFound(test_file));
        }

        // Load and parse the test file
        let source_code =
            fs::read_to_string(&test_file).map_err(|e| TestError::IoError(test_file.clone(), e))?;

        let mut results = Vec::new();

        // Run the requested test modes
        match mode {
            TestMode::Bytecode => {
                results.push(self.test_bytecode(test_name, &source_code)?);
            }
            TestMode::Result => {
                results.push(self.test_result(test_name, &source_code)?);
            }
            TestMode::All => {
                results.push(self.test_bytecode(test_name, &source_code)?);
                results.push(self.test_result(test_name, &source_code)?);
            }
        }

        Ok(results)
    }

    /// Test bytecode generation against reference file
    fn test_bytecode(&self, test_name: &str, source_code: &str) -> Result<TestResult, TestError> {
        // Parse and expand macros, capturing errors as output
        let env = Rc::new(Environment::new());
        let mut all_bytecode = String::new();

        match self.parse_and_expand(source_code, env.clone()) {
            Ok(expanded) => {
                for (i, expr) in expanded.iter().enumerate() {
                    if i > 0 {
                        all_bytecode.push_str(&format!("\n--- Expression {} ---\n", i + 1));
                    }

                    match compiler::compile(expr, source_code.to_string(), env.clone()) {
                        Ok(module) => {
                            // Format bytecode as assembly-like text using Display impl
                            let bytecode_text = format!("{}", module);
                            all_bytecode.push_str(&bytecode_text);
                        }
                        Err(e) => {
                            all_bytecode.push_str(&format!("Compile error: {:?}\n", e));
                        }
                    }
                }
            }
            Err(TestError::MacroError(msg)) => {
                all_bytecode.push_str(&format!("Macro error: {}\n", msg));
            }
            Err(TestError::ParseError(msg)) => {
                all_bytecode.push_str(&format!("Parse error: {}\n", msg));
            }
            Err(e) => return Err(e), // Other errors should still fail the test
        }

        let reference_file = self.test_dir.join(format!("{}.asm", test_name));
        self.compare_with_reference(
            test_name,
            TestMode::Bytecode,
            &all_bytecode,
            &reference_file,
        )
    }

    /// Test evaluation results against reference file
    fn test_result(&self, test_name: &str, source_code: &str) -> Result<TestResult, TestError> {
        // Create VM in non-CPS mode for regular tests
        let mut vm = VM::new_with_cps(false);

        // Parse and expand macros, capturing errors as output
        let mut results = Vec::new();
        match self.parse_and_expand(source_code, vm.current_env()) {
            Ok(expanded) => {
                for expr in expanded {
                    match compiler::compile(&expr, source_code.to_string(), vm.current_env()) {
                        Ok(module) => {
                            match vm.execute(&module) {
                                Ok(result) => {
                                    // Only collect non-Unspecified results
                                    if !matches!(result, Value::Unspecified) {
                                        results.push(format!("{}\n", result));
                                    }
                                }
                                Err(e) => {
                                    results.push(format!("Runtime error: {}\n", e));
                                }
                            }
                        }
                        Err(e) => {
                            results.push(format!("Compile error: {:?}\n", e));
                        }
                    }
                }
            }
            Err(TestError::MacroError(msg)) => {
                results.push(format!("Macro error: {}\n", msg));
            }
            Err(TestError::ParseError(msg)) => {
                results.push(format!("Parse error: {}\n", msg));
            }
            Err(e) => return Err(e), // Other errors should still fail the test
        }

        let result_text = results.join("");

        let reference_file = self.test_dir.join(format!("{}.out", test_name));
        self.compare_with_reference(test_name, TestMode::Result, &result_text, &reference_file)
    }

    /// Compare actual output with reference file, with option to update reference
    fn compare_with_reference(
        &self,
        test_name: &str,
        mode: TestMode,
        actual: &str,
        reference_file: &Path,
    ) -> Result<TestResult, TestError> {
        let expected = if reference_file.exists() {
            fs::read_to_string(reference_file)
                .map_err(|e| TestError::IoError(reference_file.to_path_buf(), e))?
        } else {
            String::new()
        };

        let passed = actual.trim() == expected.trim();

        let diff = if !passed {
            Some(self.generate_diff(&expected, actual))
        } else {
            None
        };

        // Update reference file if requested and test failed
        if self.update_references && !passed {
            fs::write(reference_file, actual)
                .map_err(|e| TestError::IoError(reference_file.to_path_buf(), e))?;

            println!("Updated reference file: {}", reference_file.display());
        }

        Ok(TestResult {
            test_name: test_name.to_string(),
            mode,
            passed,
            expected,
            actual: actual.to_string(),
            diff,
        })
    }

    /// Generate a simple diff between expected and actual output
    fn generate_diff(&self, expected: &str, actual: &str) -> String {
        let mut diff = String::new();
        diff.push_str("--- Expected ---\n");
        diff.push_str(expected);
        diff.push_str("\n--- Actual ---\n");
        diff.push_str(actual);
        diff.push_str("\n--- End Diff ---\n");
        diff
    }

    /// Run all tests in the test directory
    pub fn run_all_tests(&self, mode: TestMode) -> Result<Vec<TestResult>, TestError> {
        let mut all_results = Vec::new();

        // Find all .scm files in the test directory
        let entries = fs::read_dir(&self.test_dir)
            .map_err(|e| TestError::IoError(self.test_dir.clone(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| TestError::IoError(self.test_dir.clone(), e))?;
            let path = entry.path();

            if let Some(extension) = path.extension() {
                if extension == "scm" {
                    if let Some(stem) = path.file_stem() {
                        let test_name = stem.to_string_lossy();
                        match self.run_test(&test_name, mode.clone()) {
                            Ok(mut results) => all_results.append(&mut results),
                            Err(e) => {
                                eprintln!("Error running test {}: {:?}", test_name, e);
                            }
                        }
                    }
                }
            }
        }

        Ok(all_results)
    }
}

/// Errors that can occur during testing
#[derive(Debug)]
pub enum TestError {
    FileNotFound(PathBuf),
    IoError(PathBuf, io::Error),
    ParseError(String),
    CompileError(String),
    RuntimeError(String),
    MacroError(String),
}

impl std::fmt::Display for TestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestError::FileNotFound(path) => write!(f, "Test file not found: {}", path.display()),
            TestError::IoError(path, e) => write!(f, "IO error with {}: {}", path.display(), e),
            TestError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            TestError::CompileError(msg) => write!(f, "Compile error: {}", msg),
            TestError::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
            TestError::MacroError(msg) => write!(f, "Macro error: {}", msg),
        }
    }
}

impl std::error::Error for TestError {}

/// Print test results in a nice format
pub fn print_test_results(results: &[TestResult]) {
    let total = results.len();
    let passed = results.iter().filter(|r| r.passed).count();
    let failed = total - passed;

    println!("\n=== Test Results ===");
    println!("Total: {}, Passed: {}, Failed: {}", total, passed, failed);

    if failed > 0 {
        println!("\n=== Failed Tests ===");
        for result in results.iter().filter(|r| !r.passed) {
            println!("\n❌ {} ({:?})", result.test_name, result.mode);
            if let Some(diff) = &result.diff {
                println!("{}", diff);
            }
        }
    }

    println!("\n=== Summary ===");
    for result in results {
        let status = if result.passed { "✅" } else { "❌" };
        println!("{} {} ({:?})", status, result.test_name, result.mode);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_runner_basic() {
        // Create a temporary directory
        let temp_dir = std::env::temp_dir().join("samplescheme_test");
        fs::create_dir_all(&temp_dir).unwrap();

        // Create a simple test file
        let test_content = "(+ 1 2)";
        fs::write(temp_dir.join("simple.scm"), test_content).unwrap();

        let runner = TestRunner::new(&temp_dir);
        let results = runner.run_test("simple", TestMode::Result).unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].actual.trim(), "3");

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
