//! RulesXP - Multi-language rules expression evaluator
//!
//! This crate provides a minimalistic expression evaluator that supports both Scheme syntax
//! and JSONLogic operations with strict typing. It implements a proper subset of both
//! languages designed for reliable rule evaluation with predictable behavior.
//!
//! ## Dual Language Support
//!
//! The evaluator accepts expressions in Scheme or JSONLogic syntax:
//!
//! ```scheme
//! ;; Scheme syntax
//! (+ 1 2 3)           ; arithmetic
//! (if #t "yes" "no")  ; conditionals
//! (and #t #f)         ; boolean logic
//! (car '(1 2 3))      ; list operations
//! ```
//!
//! The same operations can be represented in JSONLogic:
//! ```json
//! {"+": [1, 2, 3]}
//! {"if": [true, "yes", "no"]}
//! {"and": [true, false]}
//! {"car": [[1, 2, 3]]}
//! ```
//!
//! ## Strict Typing
//!
//! This interpreter implements stricter semantics than standard Scheme or JSONLogic:
//! - No type coercion (numbers don't become strings, etc.)
//! - Boolean operations require actual boolean values (no "truthiness")
//! - Arithmetic overflow detection and error reporting
//! - Strict arity checking for all functions
//!
//! Any program accepted by this interpreter will give identical results in standard
//! Scheme R7RS-small or JSONLogic interpreters, but the converse is not true due to
//! our additional type safety requirements.
//!
//! ## Modules
//!
//! - `scheme`: S-expression parsing from text
//! - `evaluator`: Core expression evaluation engine
//! - `builtinops`: Built-in operations with dual-language mapping
//! - `jsonlogic`: JSONLogic format conversion and integration

use std::fmt;

/// Maximum parsing depth to prevent stack overflow attacks
/// This limits deeply nested structures in both S-expression and JSONLogic parsers
pub const MAX_PARSE_DEPTH: usize = 32;

/// Maximum evaluation depth to prevent stack overflow in recursive evaluation
/// This limits deeply nested function calls and expressions during evaluation
/// Set higher than parse depth to allow for nested function applications
pub const MAX_EVAL_DEPTH: usize = 64;

/// Represents a position (line and column) in the source input.
#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Categorizes the different kinds of parsing errors.
#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorKind {
    InvalidToken,          // An unexpected character or token was found.
    UnexpectedEndOfInput,  // The input ended prematurely.
    NumericOverflow,       // A number literal was too large or small.
    InvalidHexLiteral,     // A hexadecimal literal was malformed.
    InvalidEscapeSequence, // An unrecognized escape sequence in a string.
    UnterminatedString,    // A string literal was not closed.
    MismatchedParentheses, // Unbalanced parentheses in a list.
    TooDeeplyNested,       // The expression exceeded the maximum parse depth.
    EmptyInput,            // The input string was empty.
    TrailingInput,         // Extra characters were found after a valid expression.
    TrailingContent,       // The parser encountered content after a complete expression was parsed.
    Incomplete,            // The input is a valid prefix but is not a complete expression.
    Unsupported, // The input uses valid R7RS syntax that is not supported by this interpreter.
}

/// A structured error providing detailed information about a parsing failure.
#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    pub position: Position,
    pub kind: ParseErrorKind,
    pub message: String,
}

impl ParseError {
    /// Create a simple ParseError with no position information (for JSONLogic and CEL parsers)
    pub fn simple(kind: ParseErrorKind, message: impl Into<String>) -> Self {
        ParseError {
            position: Position::default(),
            kind,
            message: message.into(),
        }
    }

    /// Create a ParseError from just a message (defaults to InvalidToken kind, no position)
    pub fn from_message(message: impl Into<String>) -> Self {
        Self::simple(ParseErrorKind::InvalidToken, message)
    }
}

/// Error types for the interpreter
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ParseError(ParseError),
    EvalError(String),
    TypeError(String),
    UnboundVariable(String),
    ArityError {
        expected: usize,
        got: usize,
        expression: Option<String>, // Optional expression context
    },
}

impl Error {
    /// Create an ArityError without expression context
    pub fn arity_error(expected: usize, got: usize) -> Self {
        Error::ArityError {
            expected,
            got,
            expression: None,
        }
    }

    /// Create an ArityError with expression context
    pub fn arity_error_with_expr(expected: usize, got: usize, expression: String) -> Self {
        Error::ArityError {
            expected,
            got,
            expression: Some(expression),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseError(e) => write!(
                f,
                "Parse error at line {} col {}: {}",
                e.position.line, e.position.column, e.message
            ),
            Error::EvalError(msg) => write!(f, "Evaluation error: {msg}"),
            Error::TypeError(msg) => write!(f, "Type error: {msg}"),
            Error::UnboundVariable(var) => write!(f, "Unbound variable: {var}"),
            Error::ArityError {
                expected,
                got,
                expression,
            } => match expression {
                Some(expr) => write!(
                    f,
                    "Arity error in expression {expr}: expected {expected} arguments, got {got}"
                ),
                None => write!(
                    f,
                    "Arity error: function expected {expected} arguments but got {got}"
                ),
            },
        }
    }
}

pub mod ast;
pub mod builtinops;
pub mod evaluator;

#[cfg(feature = "jsonlogic")]
pub mod jsonlogic;

#[cfg(feature = "scheme")]
pub mod scheme;
