//! # R7RS Scheme Bytecode Interpreter
//!
//! A small bytecode-based Scheme interpreter targeting a subset of R7RS Scheme.
//! Uses rustyline for REPL functionality and nom for parsing.
//!
//! ## R7RS Limitations and Design Decisions
//!
//! This implementation makes several simplifications compared to full R7RS Scheme:
//!
//! ### Environment and Lexical Scoping
//! - **No `set!` support**: All variable bindings are immutable after creation
//! - **Simplified lexical scoping**: Uses runtime environment capture instead of
//!   compile-time free variable analysis (less efficient but simpler)
//! - **No `let`, `let*`, `letrec`**: Only `define` and `lambda` for bindings
//! - **Environment chaining**: Proper lexical scoping through environment chain walking
//! - **TODO: Inner defines don't work correctly**: Scoping issue with compilation vs runtime environments.
//!   Will be addressed when moving to CPS (Continuation Passing Style) transformation.
//!
//! ### Data Types
//! - **Limited numeric tower**: Only i64/u64 integers and f64 floats, no arbitrary precision
//! - **No improper lists**: Only proper lists using Vec, no dotted pairs
//! - **No characters**: Character literals not implemented
//! - **No vectors**: Vector literals not implemented  
//! - **No complex numbers**: Complex number support not implemented
//!
//! ### Control Flow
//! - **Basic tail call optimization**: Implemented but not comprehensive
//! - **No continuations**: `call/cc` and continuations not supported
//! - **No `dynamic-wind`**: Dynamic extent management not implemented
//!
//! ### Macros
//! - **Basic macro system**: Planned but not yet implemented
//! - **No syntax-rules**: Advanced macro facilities not supported
//!
//! ### I/O and Libraries
//! - **No module system**: Single global namespace
//! - **Limited I/O**: Basic REPL only, no file I/O
//! - **No standard library**: Only core procedures implemented
//!
//! These limitations are documented in tests and code comments where relevant.

pub mod builtins;
pub mod compiler;
pub mod cps;
pub mod cps_builtins;
pub mod macros;
pub mod parser;
pub mod repl;
pub mod test_runner;
pub mod value;
pub mod vm;

#[cfg(test)]
pub mod test_closure_fix;

pub use compiler::{compile, CompileError};
pub use macros::MacroExpander;
pub use parser::{parse, ParseError};
pub use value::Value;
pub use vm::{BytecodeModule, RuntimeError, VM};
