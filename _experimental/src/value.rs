use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// Core Scheme value types
///
/// ## R7RS Deviations and Limitations:
///
/// **Missing Features:**
/// - Characters: Not implemented yet
/// - Vectors: Not implemented yet  
/// - Bytevectors: Not implemented yet
/// - Ports: Not implemented yet
/// - Records: Not implemented yet
/// - Complex numbers and rationals: Only supports integers and reals
///
/// **Partial Implementations:**
/// - Numbers: Only supports i64/u64 integers and f64 floats, no arbitrary precision
/// - Lists: No support for improper lists (dotted pairs) - only proper lists using Vec
///
/// **R7RS DEVIATION:** Limited numeric tower - missing exact rationals, complex numbers, arbitrary precision
#[derive(Debug, Clone)]
pub enum Value {
    /// Boolean values (#t and #f)
    Boolean(bool),

    /// Signed exact integers
    Integer(i64),

    /// Unsigned exact integers  
    UInteger(u64),

    /// Inexact real numbers (f64)
    Real(f64),

    /// String literals
    String(String),

    /// Symbols (interned identifiers)
    Symbol(String),

    /// Proper lists only (no improper/dotted lists)
    /// Empty list is represented as empty Vec
    List(Vec<Value>),

    /// Built-in procedures
    Builtin {
        name: String,
        arity: Arity,
        func: fn(&[Value]) -> Result<Value, String>,
    },

    /// User-defined procedures (closures)
    Procedure {
        params: Vec<String>,
        body: Rc<Value>,
        env: Rc<Environment>,
        variadic: bool, // true if procedure accepts variable arguments
    },

    /// Unspecified value (returned by some procedures)
    Unspecified,
}

/// Procedure arity specification
#[derive(Debug, Clone)]
pub enum Arity {
    /// Exactly n arguments
    Exact(usize),
    /// At least n arguments  
    AtLeast(usize),
    /// Between min and max arguments (inclusive)
    Range(usize, usize),
}

/// Environment for variable bindings
#[derive(Debug, Clone)]
pub struct Environment {
    pub bindings: RefCell<HashMap<String, Value>>,
    pub parent: Option<Rc<Environment>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Environment>) -> Self {
        Environment {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent),
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: &str, value: Value) -> Result<(), String> {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.set(name, value)
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }
}

impl Value {
    /// Check if value is truthy (everything except #f is truthy in Scheme)
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    /// Check if value is a proper list
    pub fn is_proper_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    /// Convert list to vector for easier processing
    pub fn list_to_vec(&self) -> Result<Vec<Value>, String> {
        match self {
            Value::List(vec) => Ok(vec.clone()),
            _ => Err("Not a list".to_string()),
        }
    }

    /// Create a proper list from a vector of values
    pub fn vec_to_list(values: Vec<Value>) -> Value {
        Value::List(values)
    }

    /// Get the type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Integer(_) => "integer",
            Value::UInteger(_) => "unsigned-integer",
            Value::Real(_) => "real",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Builtin { .. } => "procedure",
            Value::Procedure { .. } => "procedure",
            Value::Unspecified => "unspecified",
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::UInteger(a), Value::UInteger(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Unspecified, Value::Unspecified) => true,
            // Procedures are not comparable by value in Scheme
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Integer(n) => write!(f, "{}", n),
            Value::UInteger(n) => write!(f, "{}", n),
            Value::Real(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(elements) => {
                write!(f, "(")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Value::Builtin { name, .. } => write!(f, "#<builtin:{}>", name),
            Value::Procedure { .. } => write!(f, "#<procedure>"),
            Value::Unspecified => write!(f, "#<unspecified>"),
        }
    }
}
