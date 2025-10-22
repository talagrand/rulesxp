// Parser module - converts text into AST using nom parser combinators
//
// ## R7RS Deviations and Limitations:
//
// **Missing Features:**
// - Character literals (#\a, #\space, #\newline) - not implemented
// - Vector literals (#(1 2 3)) - vectors not supported in core language
// - Improper lists (dotted pairs) - only proper lists supported
// - Complex numbers (3+4i) and rationals (1/2) - only integers and reals
// - Exact/inexact number prefixes (#e, #i) - not supported
// - Alternative radix prefixes (#b, #o, #x) - only decimal numbers
// - Advanced string escapes (\n, \t, \r, \x, \u) - only \" and \\ supported
// - Quoted symbols with | | delimiters - not supported
// - Bytevector literals (#u8()) - not supported
//
// **Partial Implementations:**
// - String escape sequences - basic support for \" and \\ only
// - Symbol character set - simplified, missing some R7RS characters
//
// **Supported Comment Syntax:**
// - Line comments: `;` to end of line
// - Block comments: `#| ... |#` (supports nesting per R7RS spec)
//
// **Error Enforcement:**
// - Character literals emit parse errors when encountered
// - Improper lists emit parse errors when encountered
// - Other limitations result in parse failures or are silently ignored

use crate::value::Value;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, none_of, one_of},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

#[derive(Debug, Clone)]
pub struct ParseError(pub String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: {}", self.0)
    }
}

impl std::error::Error for ParseError {}

/// Parse a complete Scheme expression from input
pub fn parse(input: &str) -> Result<Value, ParseError> {
    match expression(input) {
        Ok(("", value)) => Ok(value),
        Ok((remaining, _)) => Err(ParseError(format!(
            "Unexpected remaining input: {}",
            remaining
        ))),
        Err(e) => Err(ParseError(format!("Parse error: {:?}", e))),
    }
}

/// Parse multiple Scheme expressions from input, ignoring comments and empty lines
/// Returns a vector of parsed expressions
/// Preprocesses input to remove all comments before parsing
pub fn parse_multiple(input: &str) -> Result<Vec<Value>, ParseError> {
    // Preprocess to remove all comments comprehensively
    let preprocessed = remove_all_comments(input);

    let mut expressions = Vec::new();
    let mut remaining = preprocessed.as_str();
    let original_length = preprocessed.len();

    while !remaining.trim().is_empty() {
        // Skip remaining whitespace (comments already removed)
        remaining = remaining.trim_start();

        if remaining.is_empty() {
            break;
        }

        // Parse one expression
        match expression(remaining) {
            Ok((rest, value)) => {
                expressions.push(value);
                remaining = rest;
            }
            Err(e) => {
                // Calculate position for better error reporting
                let position = original_length - remaining.len();
                let lines_before = preprocessed[..position].lines().count();

                // Get context around error (up to 200 chars)
                let context_start = position.saturating_sub(100);
                let context_end = (position + 100).min(preprocessed.len());
                let context = &preprocessed[context_start..context_end];

                return Err(ParseError(format!(
                    "Parse error at position {} (around line {}): {:?}\nContext: {}",
                    position, lines_before, e, context
                )));
            }
        }
    }

    Ok(expressions)
}

/// Remove all comments from input, handling:
/// - Line comments starting with `;`
/// - Block comments `#| ... |#` (can be nested per R7RS)
/// - Comments at end of lines
/// - Comments inside multi-line expressions
/// - Preserves string literals (comments inside strings are kept)
fn remove_all_comments(input: &str) -> String {
    let mut result = String::new();
    let mut chars = input.chars().peekable();
    let mut in_string = false;
    let mut in_string_escape = false;
    let mut block_comment_depth = 0;

    while let Some(ch) = chars.next() {
        // Handle string escape sequences
        if in_string && in_string_escape {
            in_string_escape = false;
            result.push(ch);
            continue;
        }

        // Handle string state
        if ch == '"' && !in_string_escape && block_comment_depth == 0 {
            in_string = !in_string;
            result.push(ch);
            continue;
        }

        if ch == '\\' && in_string {
            in_string_escape = true;
            result.push(ch);
            continue;
        }

        // Skip processing if inside string
        if in_string {
            result.push(ch);
            continue;
        }

        // Handle block comment start: #|
        if ch == '#' && chars.peek() == Some(&'|') {
            chars.next(); // Consume '|'
            block_comment_depth += 1;
            continue;
        }

        // Handle block comment end: |#
        if ch == '|' && chars.peek() == Some(&'#') && block_comment_depth > 0 {
            chars.next(); // Consume '#'
            block_comment_depth -= 1;
            continue;
        }

        // Skip characters inside block comments
        if block_comment_depth > 0 {
            // Preserve newlines for line structure even in block comments
            if ch == '\n' {
                result.push('\n');
            }
            continue;
        }

        // Handle line comments: ;
        if ch == ';' {
            // Start of comment - skip until end of line
            for next_ch in chars.by_ref() {
                if next_ch == '\n' {
                    result.push('\n'); // Preserve newline for line structure
                    break;
                }
                // Skip comment characters
            }
            continue;
        }

        // Normal character - add to result
        result.push(ch);
    }

    result
}

/// Parse a Scheme expression (the main entry point)
fn expression(input: &str) -> IResult<&str, Value> {
    preceded(
        multispace0,
        alt((
            boolean,
            number,
            string_literal,
            character, // TODO: implement
            symbol,
            quoted,
            list,
        )),
    )(input)
}

/// Parse boolean literals (#t and #f)
fn boolean(input: &str) -> IResult<&str, Value> {
    alt((
        // Parse longer forms first to avoid partial matches
        value(Value::Boolean(true), tag("#true")),
        value(Value::Boolean(false), tag("#false")),
        value(Value::Boolean(true), tag("#t")),
        value(Value::Boolean(false), tag("#f")),
    ))(input)
}

/// Parse numeric literals (integers only)
/// **R7RS RESTRICTED:** Only supports i64 integers for simplicity
/// Missing: floats, exact/inexact prefixes, different radixes, rational numbers, complex numbers
fn number(input: &str) -> IResult<&str, Value> {
    let (input, sign) = opt(one_of("+-"))(input)?;
    let (input, number_digits) = take_while1(|c: char| c.is_ascii_digit())(input)?;

    let full_number = format!("{}{}", sign.unwrap_or('+'), number_digits);

    match full_number.parse::<i64>() {
        Ok(n) => Ok((input, Value::Integer(n))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

/// Parse string literals
/// **R7RS RESTRICTED:** Basic escape sequences supported: \", \\, \n, \t, \r
/// Missing: \x, \u escape sequences for arbitrary Unicode points
fn string_literal(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('"')(input)?;
    let (input, content) = many0(alt((
        map(tag("\\\""), |_| '"'),
        map(tag("\\\\"), |_| '\\'),
        map(tag("\\n"), |_| '\n'),
        map(tag("\\t"), |_| '\t'),
        map(tag("\\r"), |_| '\r'),
        none_of("\""),
    )))(input)?;
    let (input, _) = char('"')(input)?;

    Ok((input, Value::String(content.into_iter().collect())))
}

/// Parse character literals  
/// **R7RS DEVIATION:** Character literals not implemented
/// **NEEDS-ENFORCEMENT:** Cannot detect #\ patterns at parser level - would need lexer changes
fn character(_input: &str) -> IResult<&str, Value> {
    // Character literals like #\a, #\space, #\newline are not supported
    // This function is kept for completeness but will never match since
    // the # character is not handled by the main expression parser
    Err(nom::Err::Error(nom::error::Error::new(
        _input,
        nom::error::ErrorKind::Tag,
    )))
}

/// Parse symbols (identifiers)
/// **R7RS DEVIATION:** Simplified character set, no | | quoted symbols
fn symbol(input: &str) -> IResult<&str, Value> {
    fn is_initial(c: char) -> bool {
        c.is_ascii_alphabetic() || "!$%&*+-./:<=>?@^_~".contains(c)
    }

    fn is_subsequent(c: char) -> bool {
        is_initial(c) || c.is_ascii_digit()
    }

    map(
        recognize(pair(take_while1(is_initial), take_while(is_subsequent))),
        |s: &str| Value::Symbol(s.to_string()),
    )(input)
}

/// Parse quoted expressions 'expr
fn quoted(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('\'')(input)?;
    let (input, expr) = expression(input)?;

    // Convert 'expr to (quote expr)
    Ok((
        input,
        Value::List(vec![Value::Symbol("quote".to_string()), expr]),
    ))
}

/// Parse lists (expr1 expr2 ...) and empty list ()
fn list(input: &str) -> IResult<&str, Value> {
    delimited(
        char('('),
        |input| {
            let (input, _) = multispace0(input)?;

            // Parse list elements
            let (input, elements) = separated_list0(multispace1, expression)(input)?;
            let (input, _) = multispace0(input)?;

            // Check for improper list (dotted tail)
            let (input, tail) =
                opt(preceded(terminated(char('.'), multispace0), expression))(input)?;

            let (input, _) = multispace0(input)?;

            // Build the list structure
            let list = if tail.is_some() {
                // **R7RS DEVIATION:** Improper lists (dotted pairs) not supported
                // This would need to be handled at a higher level to emit proper error message
                // since nom parsers don't support custom error types easily
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )));
            } else {
                // Proper list
                Value::List(elements)
            };

            Ok((input, list))
        },
        char(')'),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_boolean() {
        assert_eq!(parse("#t").unwrap(), Value::Boolean(true));
        assert_eq!(parse("#f").unwrap(), Value::Boolean(false));
        assert_eq!(parse("#true").unwrap(), Value::Boolean(true));
        assert_eq!(parse("#false").unwrap(), Value::Boolean(false));
    }

    #[test]
    fn test_parse_numbers() {
        assert_eq!(parse("42").unwrap(), Value::Integer(42));
        assert_eq!(parse("-17").unwrap(), Value::Integer(-17));
        // **R7RS RESTRICTED:** Float parsing not supported, only i64 integers
        // Real number parsing removed for simplicity
    }

    #[test]
    fn test_parse_strings() {
        assert_eq!(
            parse("\"hello\"").unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(
            parse("\"hello\\\"world\"").unwrap(),
            Value::String("hello\"world".to_string())
        );
        assert_eq!(
            parse("\"path\\\\to\\\\file\"").unwrap(),
            Value::String("path\\to\\file".to_string())
        );
    }

    #[test]
    fn test_parse_symbols() {
        assert_eq!(parse("foo").unwrap(), Value::Symbol("foo".to_string()));
        assert_eq!(parse("+").unwrap(), Value::Symbol("+".to_string()));
        assert_eq!(
            parse("list->vector").unwrap(),
            Value::Symbol("list->vector".to_string())
        );
    }

    #[test]
    fn test_parse_empty_list() {
        assert_eq!(parse("()").unwrap(), Value::List(vec![]));
        assert_eq!(parse("( )").unwrap(), Value::List(vec![]));
    }

    #[test]
    fn test_parse_simple_list() {
        let result = parse("(+ 1 2)").unwrap();
        let expected = Value::List(vec![
            Value::Symbol("+".to_string()),
            Value::Integer(1),
            Value::Integer(2),
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_quoted() {
        let result = parse("'foo").unwrap();
        let expected = Value::List(vec![
            Value::Symbol("quote".to_string()),
            Value::Symbol("foo".to_string()),
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_nested_list() {
        let result = parse("((lambda (x) x) 42)").unwrap();
        // Just verify it parses without error for now
        match result {
            Value::List(_) => {}
            _ => panic!("Expected a list"),
        }
    }

    // Disabled tests showing R7RS deviations - these features are not supported
    #[test]
    #[ignore = "R7RS DEVIATION: Character literals not implemented"]
    fn test_parse_characters() {
        parse("#\\a").unwrap();
        parse("#\\space").unwrap();
        parse("#\\newline").unwrap();
    }

    #[test]
    #[ignore = "R7RS DEVIATION: Complex numbers and rationals not supported"]
    fn test_parse_complex_numbers() {
        parse("3+4i").unwrap();
        parse("1/2").unwrap();
    }

    #[test]
    #[ignore = "R7RS DEVIATION: Vectors not implemented"]
    fn test_parse_vectors() {
        parse("#(1 2 3)").unwrap();
    }

    #[test]
    #[ignore = "R7RS DEVIATION: Advanced string escapes not supported"]
    fn test_parse_advanced_string_escapes() {
        parse("\"line1\\nline2\"").unwrap();
        parse("\"tab\\there\"").unwrap();
        parse("\"unicode\\u0041\"").unwrap();
    }
}
