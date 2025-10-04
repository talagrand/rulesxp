// Debug test to check if identity is available in CPS environment
use std::env;

fn main() {
    println!("CPS Identity Debug Test");
    println!("======================");

    // Use the existing benchmark with RUST_BACKTRACE=1 for more debug info
    env::set_var("RUST_BACKTRACE", "1");

    // Just run the existing benchmark
    println!("Running existing CPS performance benchmark to see detailed error...");

    std::process::Command::new("cargo")
        .args(&["run", "--bin", "cps_performance_benchmark"])
        .status()
        .expect("Failed to run benchmark");
}
