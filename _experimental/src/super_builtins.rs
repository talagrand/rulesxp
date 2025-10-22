// Super builtins for ProcessedAST evaluation
// These builtins work with ProcessedValue instead of Value, providing
// complete isolation from the legacy Value system in SuperVM

use crate::processed_env::ProcessedEnvironment;
use crate::vm::RuntimeError;
use std::borrow::Cow;
use std::rc::Rc;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

/// Type alias for string symbols
pub type StringSymbol = DefaultSymbol;

/// Type alias for the string interner used throughout the Scheme interpreter
pub type SchemeStringInterner = StringInterner<DefaultBackend>;

/// ProcessedValue enum - arena-allocated values for SuperVM
/// **R7RS RESTRICTED:** Numeric tower simplified to i64 integers only for implementation simplicity
#[derive(Clone)]
pub enum ProcessedValue<'ast> {
    /// Boolean values (#t and #f)
    Boolean(bool),

    /// Signed exact integers (i64 only)
    Integer(i64),

    /// Interned string (for compiled/static data)
    String(StringSymbol),

    /// Owned string (for runtime-generated data)
    OwnedString(String),

    /// Interned symbol (for compiled/static data)
    Symbol(StringSymbol),

    /// Owned symbol (for runtime-generated data)
    OwnedSymbol(String),

    /// Proper lists with arena-allocated slices
    List(Cow<'ast, [ProcessedValue<'ast>]>),

    /// Built-in procedures with ProcessedValue signatures
    ResolvedBuiltin {
        name: StringSymbol,
        arity: ProcessedArity,
        func: for<'a> fn(
            &SchemeStringInterner,
            &[ProcessedValue<'a>],
        ) -> Result<ProcessedValue<'a>, RuntimeError>,
    },

    /// User-defined procedures (closures)
    Procedure {
        params: Cow<'ast, [StringSymbol]>,
        body: &'ast ProcessedValue<'ast>,
        env: Rc<ProcessedEnvironment<'ast>>,
        variadic: bool,
    },

    /// Special Forms - Direct ProcessedValue variants
    If {
        test: &'ast ProcessedValue<'ast>,
        then_branch: &'ast ProcessedValue<'ast>,
        else_branch: Option<&'ast ProcessedValue<'ast>>,
    },

    Define {
        name: StringSymbol,
        value: &'ast ProcessedValue<'ast>,
    },

    Set {
        name: StringSymbol,
        value: &'ast ProcessedValue<'ast>,
    },

    Lambda {
        params: Cow<'ast, [StringSymbol]>,
        body: &'ast ProcessedValue<'ast>,
        variadic: bool,
    },

    Quote {
        value: &'ast ProcessedValue<'ast>,
    },

    Begin {
        expressions: Cow<'ast, [ProcessedValue<'ast>]>,
    },

    /// letrec binding construct with parallel binding for mutual recursion
    Letrec {
        bindings: Cow<'ast, [(StringSymbol, ProcessedValue<'ast>)]>,
        body: &'ast ProcessedValue<'ast>,
    },

    /// letrec* binding construct with sequential left-to-right binding
    LetrecStar {
        bindings: Cow<'ast, [(StringSymbol, ProcessedValue<'ast>)]>,
        body: &'ast ProcessedValue<'ast>,
    },

    /// Unspecified value (returned by some procedures)
    Unspecified,
}

impl<'ast> std::fmt::Debug for ProcessedValue<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessedValue::Boolean(b) => write!(f, "Boolean({})", b),
            ProcessedValue::Integer(i) => write!(f, "Integer({})", i),
            ProcessedValue::String(s) => write!(f, "String({:?})", s),
            ProcessedValue::OwnedString(s) => write!(f, "OwnedString({:?})", s),
            ProcessedValue::Symbol(s) => write!(f, "Symbol({:?})", s),
            ProcessedValue::OwnedSymbol(s) => write!(f, "OwnedSymbol({:?})", s),
            ProcessedValue::List(_) => write!(f, "List(<items>)"),
            ProcessedValue::ResolvedBuiltin { name, arity, .. } => {
                write!(f, "ResolvedBuiltin(name: {:?}, arity: {:?})", name, arity)
            }
            ProcessedValue::Procedure { params, .. } => {
                write!(f, "Procedure(params: {:?})", params)
            }
            ProcessedValue::If { .. } => write!(f, "If(<test> <then> <else>)"),
            ProcessedValue::Define { name, .. } => write!(f, "Define(name: {:?})", name),
            ProcessedValue::Set { name, .. } => write!(f, "Set(name: {:?})", name),
            ProcessedValue::Lambda { params, .. } => write!(f, "Lambda(params: {:?})", params),
            ProcessedValue::Quote { .. } => write!(f, "Quote(<value>)"),
            ProcessedValue::Begin { .. } => write!(f, "Begin(<expressions>)"),
            ProcessedValue::Letrec { .. } => write!(f, "Letrec(<bindings> <body>)"),
            ProcessedValue::LetrecStar { .. } => write!(f, "LetrecStar(<bindings> <body>)"),
            ProcessedValue::Unspecified => write!(f, "Unspecified"),
        }
    }
}

/// Procedure arity specification for ProcessedValue
#[derive(Debug, Clone, Copy)]
pub enum ProcessedArity {
    /// Exactly n arguments
    Exact(usize),
    /// At least n arguments  
    AtLeast(usize),
    /// Between min and max arguments (inclusive)
    Range(usize, usize),
}

// ProcessedEnvironmentRef removed - using Rc<ProcessedEnvironment<'ast>> directly for consistency

impl<'ast> ProcessedValue<'ast> {
    /// Check if value is truthy (everything except #f is truthy in Scheme)
    pub fn is_truthy(&self) -> bool {
        !matches!(self, ProcessedValue::Boolean(false))
    }

    /// Get the type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            ProcessedValue::Boolean(_) => "boolean",
            ProcessedValue::Integer(_) => "integer",
            ProcessedValue::String(_) | ProcessedValue::OwnedString(_) => "string",
            ProcessedValue::Symbol(_) | ProcessedValue::OwnedSymbol(_) => "symbol",
            ProcessedValue::List(_) => "list",
            ProcessedValue::ResolvedBuiltin { .. } => "procedure",
            ProcessedValue::Procedure { .. } => "procedure",
            ProcessedValue::If { .. } => "if-expression",
            ProcessedValue::Define { .. } => "define-expression",
            ProcessedValue::Set { .. } => "set-expression",
            ProcessedValue::Lambda { .. } => "lambda-expression",
            ProcessedValue::Quote { .. } => "quote-expression",
            ProcessedValue::Begin { .. } => "begin-expression",
            ProcessedValue::Letrec { .. } => "letrec-expression",
            ProcessedValue::LetrecStar { .. } => "letrec*-expression",
            ProcessedValue::Unspecified => "unspecified",
        }
    }

    /// Get string value from interner for display
    pub fn resolve_string<'a>(&'a self, interner: &'a SchemeStringInterner) -> Option<&'a str> {
        match self {
            ProcessedValue::String(sym) | ProcessedValue::Symbol(sym) => interner.resolve(*sym),
            ProcessedValue::OwnedString(s) | ProcessedValue::OwnedSymbol(s) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Returns a Display adapter that can format this value with interner context
    pub fn display<'b>(
        &'b self,
        interner: &'b SchemeStringInterner,
    ) -> ProcessedValueDisplay<'b, 'ast> {
        ProcessedValueDisplay {
            value: self,
            interner,
        }
    }
}

/// Display adapter for ProcessedValue that holds interner reference
pub struct ProcessedValueDisplay<'a, 'ast> {
    value: &'a ProcessedValue<'ast>,
    interner: &'a SchemeStringInterner,
}

impl<'a, 'ast> std::fmt::Display for ProcessedValueDisplay<'a, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            ProcessedValue::Boolean(b) => write!(f, "#{}", if *b { "t" } else { "f" }),
            ProcessedValue::Integer(n) => write!(f, "{}", n),
            ProcessedValue::String(sym) => {
                let s = self.interner.resolve(*sym).unwrap_or("<unresolved>");
                write!(f, "\"{}\"", s)
            }
            ProcessedValue::OwnedString(s) => write!(f, "\"{}\"", s),
            ProcessedValue::Symbol(sym) => {
                let s = self.interner.resolve(*sym).unwrap_or("<unresolved>");
                write!(f, "{}", s)
            }
            ProcessedValue::OwnedSymbol(s) => write!(f, "{}", s),
            ProcessedValue::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item.display(self.interner))?;
                }
                write!(f, ")")
            }
            ProcessedValue::ResolvedBuiltin { name, arity, .. } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                write!(f, "#<builtin:{}:{:?}>", name_str, arity)
            }
            ProcessedValue::Procedure {
                params, variadic, ..
            } => {
                write!(f, "#<procedure")?;
                if *variadic {
                    write!(f, ":variadic")?;
                }
                write!(f, " (")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    let param_str = self.interner.resolve(*p).unwrap_or("<unresolved>");
                    write!(f, "{}", param_str)?;
                }
                if *variadic {
                    write!(f, "...")?;
                }
                write!(f, ")>")
            }
            ProcessedValue::If {
                test,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "(if {} {}",
                    test.display(self.interner),
                    then_branch.display(self.interner)
                )?;
                if let Some(else_b) = else_branch {
                    write!(f, " {}", else_b.display(self.interner))?;
                }
                write!(f, ")")
            }
            ProcessedValue::Define { name, value } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                write!(f, "(define {} {})", name_str, value.display(self.interner))
            }
            ProcessedValue::Set { name, value } => {
                let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                write!(f, "(set! {} {})", name_str, value.display(self.interner))
            }
            ProcessedValue::Lambda {
                params, variadic, ..
            } => {
                write!(f, "(lambda (")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    let param_str = self.interner.resolve(*p).unwrap_or("<unresolved>");
                    write!(f, "{}", param_str)?;
                }
                if *variadic {
                    write!(f, "...")?;
                }
                write!(f, ") <body>)")
            }
            ProcessedValue::Quote { value } => {
                write!(f, "'{}", value.display(self.interner))
            }
            ProcessedValue::Begin { expressions } => {
                write!(f, "(begin")?;
                for expr in expressions.iter() {
                    write!(f, " {}", expr.display(self.interner))?;
                }
                write!(f, ")")
            }
            ProcessedValue::Letrec { bindings, .. } => {
                write!(f, "(letrec (")?;
                for (i, (name, val)) in bindings.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                    write!(f, "[{} {}]", name_str, val.display(self.interner))?;
                }
                write!(f, ") <body>)")
            }
            ProcessedValue::LetrecStar { bindings, .. } => {
                write!(f, "(letrec* (")?;
                for (i, (name, val)) in bindings.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    let name_str = self.interner.resolve(*name).unwrap_or("<unresolved>");
                    write!(f, "[{} {}]", name_str, val.display(self.interner))?;
                }
                write!(f, ") <body>)")
            }
            ProcessedValue::Unspecified => write!(f, "#<unspecified>"),
        }
    }
}

impl<'ast> PartialEq for ProcessedValue<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ProcessedValue::Boolean(a), ProcessedValue::Boolean(b)) => a == b,
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a == b,
            (ProcessedValue::String(a), ProcessedValue::String(b)) => a == b,
            (ProcessedValue::Symbol(a), ProcessedValue::Symbol(b)) => a == b,
            (ProcessedValue::OwnedString(a), ProcessedValue::OwnedString(b)) => a == b,
            (ProcessedValue::OwnedSymbol(a), ProcessedValue::OwnedSymbol(b)) => a == b,
            // Cross-type string/symbol comparisons
            (ProcessedValue::String(_a), ProcessedValue::OwnedString(_b)) => {
                // Would need interner context to resolve - simplified for now
                false
            }
            (ProcessedValue::List(a), ProcessedValue::List(b)) => a == b,
            (ProcessedValue::Unspecified, ProcessedValue::Unspecified) => true,
            // Procedures are not comparable by value in Scheme
            _ => false,
        }
    }
}

/// Super builtin functions that work with ProcessedValue
pub mod builtin_functions {
    use super::*;

    /// Addition builtin for ProcessedValue
    /// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
    pub fn builtin_add<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Ok(ProcessedValue::Integer(0));
        }

        let mut result = 0i64;

        for arg in args {
            match arg {
                ProcessedValue::Integer(n) => {
                    result = result
                        .checked_add(*n)
                        .ok_or_else(|| RuntimeError::new("Integer overflow in addition"))?;
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: + requires integers, got {}",
                        arg.type_name()
                    )));
                }
            }
        }

        Ok(ProcessedValue::Integer(result))
    }

    /// Subtraction builtin for ProcessedValue
    /// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
    pub fn builtin_sub<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Arity error: - requires at least 1 argument",
            ));
        }

        if args.len() == 1 {
            // Negation
            match &args[0] {
                ProcessedValue::Integer(n) => n
                    .checked_neg()
                    .map(ProcessedValue::Integer)
                    .ok_or_else(|| RuntimeError::new("Integer overflow in negation")),
                _ => Err(RuntimeError::new(format!(
                    "Type error: - requires integers, got {}",
                    args[0].type_name()
                ))),
            }
        } else {
            // Subtraction
            let mut result = match &args[0] {
                ProcessedValue::Integer(n) => *n,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: - requires integers, got {}",
                        args[0].type_name()
                    )));
                }
            };

            for arg in &args[1..] {
                match arg {
                    ProcessedValue::Integer(n) => {
                        result = result
                            .checked_sub(*n)
                            .ok_or_else(|| RuntimeError::new("Integer overflow in subtraction"))?;
                    }
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "Type error: - requires integers, got {}",
                            arg.type_name()
                        )));
                    }
                }
            }

            Ok(ProcessedValue::Integer(result))
        }
    }

    /// Multiplication builtin for ProcessedValue
    /// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
    pub fn builtin_mul<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Ok(ProcessedValue::Integer(1));
        }

        let mut result = 1i64;

        for arg in args {
            match arg {
                ProcessedValue::Integer(n) => {
                    result = result
                        .checked_mul(*n)
                        .ok_or_else(|| RuntimeError::new("Integer overflow in multiplication"))?;
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Type error: * requires integers, got {}",
                        arg.type_name()
                    )));
                }
            }
        }

        Ok(ProcessedValue::Integer(result))
    }

    /// Equality builtin for ProcessedValue
    pub fn builtin_eq<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: = requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a == b,
            (ProcessedValue::Boolean(a), ProcessedValue::Boolean(b)) => a == b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: = requires integers or booleans, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    // **R7RS RESTRICTED:** Division and modulo operations removed for simplicity
    // Integer division behavior is surprising and error-prone

    /// Less than builtin for ProcessedValue
    pub fn builtin_lt<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: < requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a < b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: < requires integers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Greater than builtin for ProcessedValue
    pub fn builtin_gt<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: > requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a > b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: > requires integers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Less than or equal builtin for ProcessedValue
    pub fn builtin_le<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: <= requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a <= b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: <= requires integers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Greater than or equal builtin for ProcessedValue
    pub fn builtin_ge<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: >= requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        let result = match (&args[0], &args[1]) {
            (ProcessedValue::Integer(a), ProcessedValue::Integer(b)) => a >= b,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Type error: >= requires integers, got {} and {}",
                    args[0].type_name(),
                    args[1].type_name()
                )));
            }
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Logical not builtin for ProcessedValue
    pub fn builtin_not<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: not requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        // **R7RS SEMANTICS:** Only #f is false, everything else is true
        let result = match &args[0] {
            ProcessedValue::Boolean(false) => true,
            _ => false,
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Car builtin for ProcessedValue (first element of list)
    pub fn builtin_car<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: car requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        match &args[0] {
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    Err(RuntimeError::new(
                        "Type error: car of empty list".to_string(),
                    ))
                } else {
                    Ok(elements[0].clone())
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Type error: car requires a list, got {}",
                args[0].type_name()
            ))),
        }
    }

    /// Cdr builtin for ProcessedValue (rest of list)
    pub fn builtin_cdr<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: cdr requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        match &args[0] {
            ProcessedValue::List(elements) => {
                if elements.is_empty() {
                    Err(RuntimeError::new(
                        "Type error: cdr of empty list".to_string(),
                    ))
                } else {
                    // Return the rest of the list as a new list
                    let rest = elements[1..].to_vec();
                    Ok(ProcessedValue::List(Cow::Owned(rest)))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Type error: cdr requires a list, got {}",
                args[0].type_name()
            ))),
        }
    }

    /// Cons builtin for ProcessedValue (construct list)
    pub fn builtin_cons<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(format!(
                "Arity error: cons requires exactly 2 arguments, got {}",
                args.len()
            )));
        }

        match &args[1] {
            ProcessedValue::List(elements) => {
                let mut new_list = vec![args[0].clone()];
                new_list.extend_from_slice(elements);
                Ok(ProcessedValue::List(Cow::Owned(new_list)))
            }
            _ => {
                // **R7RS RESTRICTED:** Improper lists (dotted pairs) not supported
                // R7RS requires: (cons 1 2) → (1 . 2) [improper list]
                // We emit an error instead of silently creating wrong structure
                Err(RuntimeError::new(format!(
                    "R7RS RESTRICTED: cons second argument must be a list (improper lists not supported). \
                     Got {} for second argument. Use (list {} {}) for a proper 2-element list.",
                    args[1].type_name(),
                    args[0].display(_interner),
                    args[1].display(_interner)
                )))
            }
        }
    }

    /// List builtin for ProcessedValue (construct list from arguments)
    pub fn builtin_list<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        // list can take any number of arguments (including zero)
        Ok(ProcessedValue::List(Cow::Owned(args.to_vec())))
    }

    /// Display a value to stdout
    pub fn builtin_display<'a>(
        interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: display requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        match &args[0] {
            ProcessedValue::String(s) => {
                if let Some(val) = interner.resolve(*s) {
                    print!("{}", val);
                } else {
                    return Err(RuntimeError::new(format!(
                        "display: failed to resolve interned string"
                    )));
                }
            }
            ProcessedValue::OwnedString(s) => print!("{}", s),
            ProcessedValue::Integer(i) => print!("{}", i),
            ProcessedValue::Boolean(b) => print!("{}", if *b { "#t" } else { "#f" }),
            // **R7RS DEVIATION:** Display for procedures, lists, etc., is not specified and shows a debug-like format.
            other => print!("{:?}", other),
        }

        Ok(ProcessedValue::Unspecified)
    }

    /// Null predicate for ProcessedValue (check if value is empty list)
    pub fn builtin_null<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: null? requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        let result = match &args[0] {
            ProcessedValue::List(elements) => elements.is_empty(),
            _ => false,
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// Pair predicate - check if value is a non-empty list
    pub fn builtin_pair_p<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: pair? requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        let result = match &args[0] {
            ProcessedValue::List(elements) => !elements.is_empty(),
            _ => false,
        };

        Ok(ProcessedValue::Boolean(result))
    }

    /// List predicate - check if value is a list (empty or non-empty)
    pub fn builtin_list_p<'a>(
        _interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::new(format!(
                "Arity error: list? requires exactly 1 argument, got {}",
                args.len()
            )));
        }

        let result = matches!(&args[0], ProcessedValue::List(_));

        Ok(ProcessedValue::Boolean(result))
    }

    /// Error builtin - raises a runtime error with a message
    pub fn builtin_error<'a>(
        interner: &SchemeStringInterner,
        args: &[ProcessedValue<'a>],
    ) -> Result<ProcessedValue<'a>, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new("error: requires at least 1 argument"));
        }

        // Build error message from all arguments
        let mut message = String::new();
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                message.push(' ');
            }
            match arg {
                ProcessedValue::String(s) | ProcessedValue::Symbol(s) => {
                    if let Some(val) = interner.resolve(*s) {
                        message.push_str(val);
                    } else {
                        message.push_str("<unresolved>");
                    }
                }
                ProcessedValue::OwnedString(s) | ProcessedValue::OwnedSymbol(s) => {
                    message.push_str(s);
                }
                ProcessedValue::Integer(n) => {
                    message.push_str(&n.to_string());
                }
                ProcessedValue::Boolean(b) => {
                    message.push_str(if *b { "#t" } else { "#f" });
                }
                other => {
                    message.push_str(&format!("{:?}", other));
                }
            }
        }

        Err(RuntimeError::new(message))
    }

    // **R7RS RESTRICTED:** The following builtin functions are not implemented:
    // - Boolean operators: and, or (should be special forms, not functions)
    // - Type predicates: null?, pair?, number?, string?, symbol?, boolean?
    // - Additional list operations: length, append, reverse, memq, assq
    // - String operations: string-length, string-ref, string-set!, substring
    // - Vector operations: vector, vector-ref, vector-set!, vector-length
    // - I/O operations: read, write, display, newline
    // - Arithmetic: mod, abs, min, max, floor, ceiling, round, truncate
    // - Character operations: char=?, char<?, char>?, char->integer, integer->char
    // - Conversion functions: number->string, string->number, symbol->string, string->symbol
    //
    // **R7RS DEVIATION:** cons should support improper lists (dotted pairs) but currently
    // only creates proper lists when second argument is not a list.
    //
    // Current SuperVM builtin coverage:
    // ✅ Arithmetic: +, -, *, /, = (5/5 core operations)
    // ✅ Comparisons: <, >, <=, >= (4/4 core comparisons)
    // ✅ Logical: not (1/1 core logical function)
    // ✅ Lists: car, cdr, cons, list (4/6 core list operations)
    // ❌ Remaining: 50+ additional R7RS builtin functions not implemented
}

/// Create super builtins for ProcessedAST evaluation
pub fn create_super_builtins() -> Vec<(StringSymbol, ProcessedValue<'static>)> {
    // This will be implemented once we have the ProcessedAST structure
    // For now, return empty vec as placeholder
    vec![]
}
