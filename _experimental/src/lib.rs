//! # R7RS Scheme Interpreter
//!
//! A Scheme interpreter implementing a subset of R7RS Scheme with multiple evaluation engines.
//! Uses rustyline for REPL functionality and nom for parsing.
//!
//! ## SuperVM Evaluation Engine
//!
//! The primary evaluation engine is **SuperVM** (see `super_vm.rs`), which provides:
//! - **Two evaluation modes**: SuperDirectVM (recursive) and SuperStackVM (iterative with TCO)
//! - **Arena-allocated AST**: ProcessedValue with string interning for efficiency
//! - **Proper tail call optimization**: Stack-safe execution in SuperStackVM
//! - **Immutable environment chains**: Fast environment extension via Rc cloning
//!
//! ### SuperVM Special Forms (Fully Implemented)
//! - `if` - Conditional expressions with R7RS truthiness (#f is false, all else true)
//! - `define` - Variable and function definitions (all bindings mutable via Rc<RefCell<>>)
//! - `set!` - Variable mutation (works on define'd vars and lambda parameters)
//! - `lambda` - Lexical closures with environment capture
//! - `quote` - Literal data without evaluation
//! - `begin` - Sequential expression evaluation
//! - `letrec` - Mutual recursion via mutable cell back-patching
//! - Function application - Both builtin and user-defined procedures
//!
//! ### SuperVM Limitations (R7RS RESTRICTED)
//! - **Numeric tower**: Only i64 integers (no floats, rationals, or arbitrary precision)
//! - **Variadic functions**: Only fully variadic `(lambda args body)` form supported
//!   - Dot notation `(lambda (a b . rest) body)` not implemented
//! - **Continuations**: No `call/cc` or `dynamic-wind` support
//! - **Data types**: No vectors, bytevectors, characters, or improper lists
//! - **Modules**: Single global namespace, no R7RS library system
//!
//! ### SuperVM Derived Forms (Handled by Macro System)
//! These must be macro-expanded before ProcessedAST compilation:
//! - `and`, `or`, `when`, `unless`, `cond`, `case`
//! - `let`, `let*`, `do`
//! - See `prelude/macros.scm` for macro definitions

pub mod builtins;
pub mod compiler;
pub mod macros;
pub mod parser;
pub mod processed_ast;
pub mod processed_env;
pub mod repl;
pub mod super_builtins;
pub mod super_vm;
pub mod test_runner;
pub mod value;
pub mod vm;

pub use compiler::{compile, CompileError};
pub use macros::MacroExpander;
pub use parser::{parse, ParseError};
pub use processed_ast::{CustomBuiltin, ProcessedAST, ProcessedCompileError};
pub use super_builtins::{ProcessedArity, ProcessedValue, SchemeStringInterner};
pub use super_vm::{SuperDirectVM, SuperStackVM};
pub use value::Value;
pub use vm::{BytecodeModule, RuntimeError, VM};
