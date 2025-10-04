// Test runner binary for Scheme interpreter tests
use samplescheme::test_runner::{print_test_results, TestMode, TestRunner};
use std::env;
use std::path::PathBuf;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Default test directory
    let test_dir = PathBuf::from("tests");

    // Check if UPDATE_TEST_REFERENCES environment variable is set
    if env::var("UPDATE_TEST_REFERENCES").is_ok() {
        println!(
            "📝 UPDATE_TEST_REFERENCES is set - will update reference files for failing tests"
        );
    }

    let runner = TestRunner::new(&test_dir);

    match args.len() {
        1 => {
            // Run all tests with all modes
            println!("Running all tests in {} directory...", test_dir.display());
            match runner.run_all_tests(TestMode::All) {
                Ok(results) => {
                    print_test_results(&results);

                    // Exit with error code if any tests failed
                    let failed_count = results.iter().filter(|r| !r.passed).count();
                    if failed_count > 0 {
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Error running tests: {}", e);
                    std::process::exit(1);
                }
            }
        }
        2 => {
            // Run specific test with all modes
            let test_name = &args[1];
            println!("Running test '{}'...", test_name);

            match runner.run_test(test_name, TestMode::All) {
                Ok(results) => {
                    print_test_results(&results);

                    let failed_count = results.iter().filter(|r| !r.passed).count();
                    if failed_count > 0 {
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Error running test '{}': {}", test_name, e);
                    std::process::exit(1);
                }
            }
        }
        3 => {
            // Run specific test with specific mode
            let test_name = &args[1];
            let mode_str = &args[2];

            let mode = match mode_str.to_lowercase().as_str() {
                "bytecode" => TestMode::Bytecode,
                "result" => TestMode::Result,
                "all" => TestMode::All,
                _ => {
                    eprintln!(
                        "Invalid test mode '{}'. Valid modes: bytecode, result, all",
                        mode_str
                    );
                    print_usage();
                    std::process::exit(1);
                }
            };

            println!("Running test '{}' with mode '{:?}'...", test_name, mode);

            match runner.run_test(test_name, mode) {
                Ok(results) => {
                    print_test_results(&results);

                    let failed_count = results.iter().filter(|r| !r.passed).count();
                    if failed_count > 0 {
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Error running test '{}': {}", test_name, e);
                    std::process::exit(1);
                }
            }
        }
        _ => {
            print_usage();
            std::process::exit(1);
        }
    }
}

fn print_usage() {
    println!("Usage:");
    println!("  test_runner                    - Run all tests with all modes");
    println!("  test_runner <test_name>        - Run specific test with all modes");
    println!("  test_runner <test_name> <mode> - Run specific test with specific mode");
    println!();
    println!("Modes: bytecode, result, all");
    println!();
    println!("Environment variables:");
    println!("  UPDATE_TEST_REFERENCES=1       - Update reference files for failing tests");
    println!();
    println!("Examples:");
    println!("  test_runner simple");
    println!("  test_runner arithmetic result");
    println!("  UPDATE_TEST_REFERENCES=1 test_runner factorial");
}
