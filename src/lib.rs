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

/// Categorizes the different kinds of parsing errors.
#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorKind {
    /// Invalid or unexpected syntax (bad tokens, malformed expressions)
    InvalidSyntax,
    /// Input ended before the expression was complete (EOF, unterminated string, unclosed parens)
    Incomplete,
    /// Expression nesting exceeded the maximum parse depth
    TooDeeplyNested,
    /// Extra input found after a complete, valid expression
    TrailingContent,
    /// Valid language syntax that is intentionally not supported in this implementation
    Unsupported,
    /// Implementation-imposed limit exceeded (depth, integer overflow, etc.)
    ImplementationLimit,
}

/// A structured error providing detailed information about a parsing failure.
#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub message: String,
    /// Context snippet from the input showing where the error occurred (max 100 chars)
    pub context: Option<String>,
    /// The problematic token or character encountered, if identifiable
    pub found: Option<String>,
}

impl ParseError {
    /// Create a ParseError with all fields
    pub fn new(
        kind: ParseErrorKind,
        message: impl Into<String>,
        context: Option<String>,
        found: Option<String>,
    ) -> Self {
        ParseError {
            kind,
            message: message.into(),
            context,
            found,
        }
    }

    /// Create a simple ParseError with a kind and message but no context
    pub fn from_message(kind: ParseErrorKind, message: impl Into<String>) -> Self {
        Self::new(kind, message, None, None)
    }

    /// Create a ParseError with context extracted from input at a given offset
    pub fn with_context(
        kind: ParseErrorKind,
        message: impl Into<String>,
        input: &str,
        error_offset: usize,
    ) -> Self {
        Self::with_context_and_found(kind, message, input, error_offset, None)
    }

    /// Create a ParseError with context and found token
    pub fn with_context_and_found(
        kind: ParseErrorKind,
        message: impl Into<String>,
        input: &str,
        error_offset: usize,
        found: Option<String>,
    ) -> Self {
        const MAX_CONTEXT: usize = 100;

        // Calculate start position: try to show some context before the error
        let context_start = error_offset.saturating_sub(20);

        // Extract context using chars().take() for simplicity
        let context_str: String = input
            .chars()
            .skip(context_start)
            .take(MAX_CONTEXT)
            .collect();

        // Add ellipsis if we truncated
        let mut display_context = String::new();
        if context_start > 0 {
            display_context.push_str("[...]");
        }
        display_context.push_str(&context_str);
        if context_start + context_str.len() < input.len() {
            display_context.push_str("[...]");
        }

        // Replace newlines with visible markers for better error display
        let display_context = display_context.replace('\n', "\\n").replace('\r', "");

        Self::new(kind, message, Some(display_context), found)
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
            Error::ParseError(e) => {
                write!(f, "ParseError: {}", e.message)?;
                if let Some(found) = &e.found {
                    write!(f, "\nFound: {found}")?;
                }
                if let Some(context) = &e.context {
                    write!(f, "\nContext: {context}")?;
                }
                Ok(())
            }
            Error::EvalError(msg) => write!(f, "EvaluationError: {msg}"),
            Error::TypeError(msg) => write!(f, "Type error: {msg}"),
            Error::UnboundVariable(var) => write!(f, "Unbound variable: {var}"),
            Error::ArityError {
                expected,
                got,
                expression,
            } => match expression {
                Some(expr) => write!(
                    f,
                    "ArityError: expression {expr}: expected {expected} arguments, got {got}"
                ),
                None => write!(
                    f,
                    "ArityError: function expected {expected} arguments but got {got}"
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
