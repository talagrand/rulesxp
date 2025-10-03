use samplescheme::repl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("SampleScheme v0.1.0 - A minimal R7RS Scheme interpreter");
    println!("Type (exit) or Ctrl+C to quit\n");

    repl::run()
}
