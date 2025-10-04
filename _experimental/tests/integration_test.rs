use samplescheme::test_runner::{TestMode, TestRunner};
use std::fs;
use std::path::Path;

#[test]
fn test_all_scheme_files() {
    let tests_dir = Path::new("tests");
    if !tests_dir.exists() {
        panic!("Tests directory not found");
    }

    let test_runner = TestRunner::new(tests_dir);
    let mut all_passed = true;
    let mut test_count = 0;

    // Find all .scm files in the tests directory
    let entries = fs::read_dir(tests_dir).expect("Failed to read tests directory");

    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("scm") {
            let test_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown");

            println!("Running test: {}", test_name);
            test_count += 1;

            match test_runner.run_test(test_name, TestMode::All) {
                Ok(results) => {
                    for result in &results {
                        if !result.passed {
                            println!(
                                "  FAILED: {:?} - Expected: {}, Actual: {}",
                                result.mode, result.expected, result.actual
                            );
                            if let Some(diff) = &result.diff {
                                println!("    Diff: {}", diff);
                            }
                            all_passed = false;
                        } else {
                            println!("  PASSED: {:?}", result.mode);
                        }
                    }
                }
                Err(e) => {
                    println!("  ERROR: Failed to run test - {}", e);
                    all_passed = false;
                }
            }
            println!();
        }
    }

    if test_count == 0 {
        panic!("No .scm test files found in tests directory");
    }

    if !all_passed {
        panic!("Some tests failed. See output above for details.");
    }

    println!("All {} scheme test files passed!", test_count);
}
