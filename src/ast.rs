/*
 This module defines the core Abstract Syntax Tree (AST) types and helper functions
 for representing values in the interpreter. The main enum, [`Value`], covers
 all Scheme data types, including numbers, symbols, strings, booleans, lists, built-in
 and user-defined functions, and precompiled operations. Ergonomic helper functions
 such as [`val`], [`sym`], and [`nil`] are provided for convenient AST construction
 in both code and tests. The module also implements conversion traits for common Rust
 types, making it easy to build Values from Rust literals, arrays, slices, and
 vectors. Equality and display logic are customized to match Scheme semantics, including
 round-trip compatibility for precompiled operations.
*/

use crate::Error;
use crate::builtinops::BuiltinOp;
use crate::evaluator::intooperation::OperationFn;

/// Type alias for number values in interpreter
/// Using i32 instead of i64 to reserve upper bits for future type tags or NaN-boxing optimizations
pub(crate) type NumberType = i32;

/// Checks if a given string is a valid Scheme symbol using the R7RS grammar, including
/// peculiar identifiers like `+`, `-`, `...`, and the special `->` prefix.
/// R7RS-RESTRICTED: This parser does not support the `|symbol with spaces|` syntax.
/// It also enforces case-sensitivity, which is a permitted implementation choice.
/// R7RS-RESTRICTED: The set of characters permitted is ASCII only for now,
/// as R7RS has requirements on initials/subsequents that do not match Rust's native character categories.
pub fn is_symbol_valid(s: &str) -> bool {
    // Case 1: Check for peculiar identifiers.
    // These are valid only if they are the entire symbol.
    if s == "+" || s == "-" || s == "..." {
        return true;
    }

    // R7RS §7.1.1 <initial> and <subsequent> character sets (ASCII-only version).
    let is_initial = |c: char| c.is_ascii_alphabetic() || "!$%&*/:<=>?^_~".contains(c);
    let is_subsequent = |c: char| is_initial(c) || c.is_ascii_digit() || "+-.@".contains(c);

    // Case 2: Check for regular identifiers.
    let mut chars = s.chars();

    // The first part of the symbol must be either a valid initial character
    // or the special "->" prefix.
    if s.starts_with("->") {
        chars.next();
        chars.next();
        // All remaining characters must be subsequent characters.
        chars.all(is_subsequent)
    } else if let Some(first) = chars.next()
        && is_initial(first)
    {
        // All remaining characters must be subsequent characters.
        chars.all(is_subsequent)
    } else {
        false
    }
}

/// Core AST type in interpreter
///
/// Note: PrecompiledOps (optimized s-expressions) don't equality-compare to dynamically
/// generated unoptimized s-expressions. However, since no expression can *return* a
/// PrecompiledOp (they're consumed during evaluation), this is not a concern for user code.
///
/// To build an AST, use the ergonomic helper functions:
/// - `val(42)` for values, `sym("name")` for symbols, `nil()` for empty lists
/// - `val([1, 2, 3])` for homogeneous lists
/// - `val(vec![sym("op"), val(42)])` for mixed lists
#[derive(Clone)]
pub enum Value {
    /// Numbers (integers only)
    Number(NumberType),
    /// Symbols (identifiers)
    Symbol(String),
    /// String literals
    String(String),
    /// Boolean values
    Bool(bool),
    /// Lists (including proper and improper lists, empty list represents nil)
    List(Vec<Value>),
    /// Pre-compiled operations with their arguments (optimized during parsing)
    PrecompiledOp {
        op: &'static BuiltinOp,
        op_id: String,
        args: Vec<Value>,
    },
    /// Built-in functions (used when called through Symbol, not pre-optimized due to dynamism)
    /// Uses id string for equality comparison instead of function pointer
    BuiltinFunction {
        id: String,
        // Stored as an Arc to allow dynamic wrapping of typed Rust functions/closures.
        // Trait object enables registering strongly typed functions (e.g. fn(i64, i64)->i64)
        // that are automatically converted to the canonical evaluator signature.
        func: std::sync::Arc<OperationFn>,
    },
    /// User-defined functions (params, body, closure env)
    Function {
        params: Vec<String>,
        body: Box<Value>,
        env: crate::evaluator::Environment,
    },
    /// Unspecified values (e.g., return value of define)
    /// These values never equal themselves or any other value
    Unspecified,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "Number({n})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::String(s) => write!(f, "String(\"{s}\")"),
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::List(list) => {
                write!(f, "List(")?;
                for (i, v) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v:?}")?;
                }
                write!(f, ")")
            }
            Value::PrecompiledOp { op_id, args, .. } => {
                write!(f, "PrecompiledOp({op_id}, args=[")?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{a:?}")?;
                }
                write!(f, "])")
            }
            Value::BuiltinFunction { id, .. } => write!(f, "BuiltinFunction({id})"),
            Value::Function { params, body, .. } => {
                write!(f, "Function(params={params:?}, body={body:?})")
            }
            Value::Unspecified => write!(f, "Unspecified"),
        }
    }
}

// From trait implementations for Value - enables .into() conversion
impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_owned())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

macro_rules! impl_from_integer {
    ($int_type:ty) => {
        impl From<$int_type> for Value {
            fn from(n: $int_type) -> Self {
                Value::Number(n as NumberType)
            }
        }
    };
}

// Generate From implementations for all integer types
impl_from_integer!(i8);
impl_from_integer!(i16);
impl_from_integer!(i32);
impl_from_integer!(u8);
impl_from_integer!(u16);
impl_from_integer!(u32);

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::List(v.into_iter().map(|x| x.into()).collect())
    }
}

impl<T: Into<Value>, const N: usize> From<[T; N]> for Value {
    fn from(arr: [T; N]) -> Self {
        Value::List(arr.into_iter().map(|x| x.into()).collect())
    }
}

impl<T: Into<Value> + Clone> From<&[T]> for Value {
    fn from(slice: &[T]) -> Self {
        Value::List(slice.iter().cloned().map(|x| x.into()).collect())
    }
}

// Fallible conversions from `Value` back into primitive Rust types.

impl std::convert::TryInto<NumberType> for Value {
    type Error = Error;

    fn try_into(self) -> Result<NumberType, Error> {
        if let Value::Number(n) = self {
            Ok(n)
        } else {
            Err(Error::TypeError("expected number".into()))
        }
    }
}

impl std::convert::TryInto<bool> for Value {
    type Error = Error;

    fn try_into(self) -> Result<bool, Error> {
        if let Value::Bool(b) = self {
            Ok(b)
        } else {
            Err(Error::TypeError("expected boolean".into()))
        }
    }
}

///   Helper function for creating symbols - works great in mixed lists!
///   Accepts both &str and String via Into<&str>
#[cfg_attr(not(test), expect(dead_code))]
pub(crate) fn sym<S: AsRef<str>>(name: S) -> Value {
    Value::Symbol(name.as_ref().to_owned())
}

/// Helper function for creating Values - works great in mixed lists!
/// Accepts any type that can be converted to Value
#[cfg_attr(not(test), expect(dead_code))]
pub(crate) fn val<T: Into<Value>>(value: T) -> Value {
    value.into()
}

/// Helper function for creating empty lists (nil) - follows Lisp/Scheme conventions
/// In Lisp, nil represents the empty list
#[cfg_attr(not(test), expect(dead_code))]
pub(crate) fn nil() -> Value {
    Value::List(vec![])
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::String(s) => {
                write!(f, "\"")?;
                for ch in s.chars() {
                    match ch {
                        '"' => write!(f, "\\\"")?,
                        '\\' => write!(f, "\\\\")?,
                        '\n' => write!(f, "\\n")?,
                        '\t' => write!(f, "\\t")?,
                        '\r' => write!(f, "\\r")?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            Value::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Value::List(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::BuiltinFunction { id, .. } => write!(f, "#<builtin-function:{id}>"),
            Value::PrecompiledOp { .. } => {
                // Display PrecompiledOp as parseable list form for round-trip compatibility
                write!(f, "{}", self.to_uncompiled_form())
            }
            Value::Function { .. } => write!(f, "#<function>"),
            Value::Unspecified => write!(f, "#<unspecified>"),
        }
    }
}

impl Value {
    /// Convert PrecompiledOp back to List form for display purposes
    pub(crate) fn to_uncompiled_form(&self) -> Value {
        match self {
            Value::PrecompiledOp { op, args, .. } => {
                let mut elements = vec![Value::Symbol(op.scheme_id.to_owned())];
                for arg in args {
                    elements.push(arg.to_uncompiled_form()); // Recursively convert nested PrecompiledOps
                }
                Value::List(elements)
            }
            Value::List(elements) => {
                // Recursively convert nested elements
                Value::List(
                    elements
                        .iter()
                        .map(|elem| elem.to_uncompiled_form())
                        .collect(),
                )
            }
            other => other.clone(), // Leave other types unchanged
        }
    }

    /// Check if a value represents nil (empty list)
    pub(crate) fn is_nil(&self) -> bool {
        matches!(self, Value::List(list) if list.is_empty())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (
                Value::PrecompiledOp {
                    op_id: id1,
                    args: args1,
                    ..
                },
                Value::PrecompiledOp {
                    op_id: id2,
                    args: args2,
                    ..
                },
            ) => {
                // Compare PrecompiledOps by id string, not function pointer
                id1 == id2 && args1 == args2
            }
            (Value::BuiltinFunction { id: id1, .. }, Value::BuiltinFunction { id: id2, .. }) => {
                // Compare BuiltinFunctions by id string, not function pointer
                id1 == id2
            }
            (
                Value::Function {
                    params: p1,
                    body: b1,
                    env: e1,
                },
                Value::Function {
                    params: p2,
                    body: b2,
                    env: e2,
                },
            ) => p1 == p2 && b1 == b2 && e1 == e2,
            (Value::Unspecified, _) | (_, Value::Unspecified) => false, // Unspecified never equals anything
            _ => false, // Different variants are never equal
        }
    }
}

#[cfg(test)]
mod helper_function_tests {
    use super::*;

    #[test]
    fn test_helper_functions_data_driven() {
        // Test cases as (Value, Value) tuples: (helper_result, expected_value)
        let test_cases = vec![
            // Basic numbers
            (val(42), Value::Number(42)),
            (val(-17), Value::Number(-17)),
            (val(-0), Value::Number(0)),
            // Different integer types from macro
            // R7RS-RESTRICTED: Limited to 32-bit signed integers (values must fit in i32)
            (val(2147483647i32), Value::Number(2147483647)), // i32::MAX
            (val(-2147483648i32), Value::Number(-2147483648)), // i32::MIN
            (val(255u8), Value::Number(255)),
            (val(-128i8), Value::Number(-128)),
            (val(32767i16), Value::Number(32767)),   // i16::MAX
            (val(-32768i16), Value::Number(-32768)), // i16::MIN
            (val(NumberType::MAX), Value::Number(NumberType::MAX)),
            (val(NumberType::MIN), Value::Number(NumberType::MIN)),
            // Basic booleans
            (val(true), Value::Bool(true)),
            (val("hello"), Value::String("hello".to_owned())),
            (val(""), Value::String(String::new())),
            // Sym, from both &str and String
            (sym("foo-bar?"), Value::Symbol("foo-bar?".to_owned())),
            (sym("-"), Value::Symbol("-".to_owned())),
            (sym(String::from("test")), Value::Symbol("test".to_owned())),
            // Empty list (nil)
            (nil(), Value::List(vec![])),
            // Lists from arrays and vecs of primitives
            (
                val([1, 2, 3]),
                Value::List(vec![Value::Number(1), Value::Number(2), Value::Number(3)]),
            ),
            (
                val(["hello", "world"]),
                Value::List(vec![
                    Value::String("hello".to_owned()),
                    Value::String("world".to_owned()),
                ]),
            ),
            (
                val([true, false, true]),
                Value::List(vec![
                    Value::Bool(true),
                    Value::Bool(false),
                    Value::Bool(true),
                ]),
            ),
            // Mixed type lists using helper functions
            (
                val(vec![sym("operation"), val(42), val("result"), val(true)]),
                Value::List(vec![
                    Value::Symbol("operation".to_owned()),
                    Value::Number(42),
                    Value::String("result".to_owned()),
                    Value::Bool(true),
                ]),
            ),
        ];

        run_helper_function_tests(test_cases);
    }

    /// Helper function to run data-driven tests for helper functions
    fn run_helper_function_tests(test_cases: Vec<(Value, Value)>) {
        for (i, (actual, expected)) in test_cases.iter().enumerate() {
            assert!(
                !(actual != expected),
                "Test case {} failed:\n  Expected: {:?}\n  Got: {:?}",
                i + 1,
                expected,
                actual
            );
        }
    }

    #[test]
    fn test_unspecified_values() {
        // Unspecified never equals anything, including itself
        let unspec = Value::Unspecified;
        assert_ne!(unspec, unspec);
        assert_ne!(unspec, Value::Unspecified);
        assert_ne!(unspec, val(42));
    }
}
