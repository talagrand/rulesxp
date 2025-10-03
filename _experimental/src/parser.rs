use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while},
    character::complete::{char, multispace0, multispace1, none_of, one_of},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};
use crate::value::Value;

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
        Ok((remaining, _)) => Err(ParseError(format!("Unexpected remaining input: {}", remaining))),
        Err(e) => Err(ParseError(format!("Parse error: {:?}", e))),
    }
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
        ))
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

/// Parse numeric literals (integers and reals)
/// **R7RS Limitation**: Only supports basic decimal integers and floats
/// Missing: exact/inexact prefixes, different radixes, rational numbers, complex numbers
fn number(input: &str) -> IResult<&str, Value> {
    let (input, sign) = opt(one_of("+-"))(input)?;
    let (input, number_str) = recognize(pair(
        take_while1(|c: char| c.is_ascii_digit()),
        opt(pair(char('.'), take_while1(|c: char| c.is_ascii_digit())))
    ))(input)?;
    
    let full_number = format!("{}{}", sign.unwrap_or('+'), number_str);
    
    if number_str.contains('.') {
        match full_number.parse::<f64>() {
            Ok(n) => Ok((input, Value::Real(n))),
            Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Float))),
        }
    } else {
        match full_number.parse::<i64>() {
            Ok(n) => Ok((input, Value::Integer(n))),
            Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Digit))),
        }
    }
}

/// Parse string literals
/// **R7RS Limitation**: Basic escape sequences only (\" and \\)
/// Missing: \n, \t, \r, \x, \u escape sequences
fn string_literal(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('"')(input)?;
    let (input, content) = many0(alt((
        map(tag("\\\""), |_| '"'),
        map(tag("\\\\"), |_| '\\'),
        none_of("\""),
    )))(input)?;
    let (input, _) = char('"')(input)?;
    
    Ok((input, Value::String(content.into_iter().collect())))
}

/// Parse character literals  
/// **R7RS Limitation**: Not implemented yet
fn character(_input: &str) -> IResult<&str, Value> {
    // TODO: Implement character literals like #\a, #\space, #\newline
    Err(nom::Err::Error(nom::error::Error::new(_input, nom::error::ErrorKind::Tag)))
}

/// Parse symbols (identifiers)
/// **R7RS Limitation**: Simplified character set, no | | quoted symbols
fn symbol(input: &str) -> IResult<&str, Value> {
    fn is_initial(c: char) -> bool {
        c.is_ascii_alphabetic() || "!$%&*+-./:<=>?@^_~".contains(c)
    }
    
    fn is_subsequent(c: char) -> bool {
        is_initial(c) || c.is_ascii_digit()
    }
    
    map(
        recognize(pair(
            take_while1(is_initial),
            take_while(is_subsequent)
        )),
        |s: &str| Value::Symbol(s.to_string())
    )(input)
}

/// Parse quoted expressions 'expr
fn quoted(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('\'')(input)?;
    let (input, expr) = expression(input)?;
    
    // Convert 'expr to (quote expr)
    Ok((input, Value::List(vec![
        Value::Symbol("quote".to_string()),
        expr
    ])))
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
            let (input, tail) = opt(preceded(
                terminated(char('.'), multispace0),
                expression
            ))(input)?;
            
            let (input, _) = multispace0(input)?;
            
            // Build the list structure
            let list = if tail.is_some() {
                // Improper lists not supported - return error
                return Err(nom::Err::Error(nom::error::Error::new(
                    input, nom::error::ErrorKind::Tag
                )));
            } else {
                // Proper list
                Value::List(elements)
            };
            
            Ok((input, list))
        },
        char(')')
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
        assert_eq!(parse("3.14").unwrap(), Value::Real(3.14));
        assert_eq!(parse("-2.5").unwrap(), Value::Real(-2.5));
    }

    #[test]
    fn test_parse_strings() {
        assert_eq!(parse("\"hello\"").unwrap(), Value::String("hello".to_string()));
        assert_eq!(parse("\"hello\\\"world\"").unwrap(), Value::String("hello\"world".to_string()));
        assert_eq!(parse("\"path\\\\to\\\\file\"").unwrap(), Value::String("path\\to\\file".to_string()));
    }

    #[test] 
    fn test_parse_symbols() {
        assert_eq!(parse("foo").unwrap(), Value::Symbol("foo".to_string()));
        assert_eq!(parse("+").unwrap(), Value::Symbol("+".to_string()));
        assert_eq!(parse("list->vector").unwrap(), Value::Symbol("list->vector".to_string()));
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
            Value::Integer(2)
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_quoted() {
        let result = parse("'foo").unwrap();
        let expected = Value::List(vec![
            Value::Symbol("quote".to_string()),
            Value::Symbol("foo".to_string())
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_nested_list() {
        let result = parse("((lambda (x) x) 42)").unwrap();
        // Just verify it parses without error for now
        match result {
            Value::List(_) => {},
            _ => panic!("Expected a list"),
        }
    }

    // Disabled tests showing R7RS limitations
    #[test]
    #[ignore = "Character literals not implemented"]
    fn test_parse_characters() {
        parse("#\\a").unwrap();
        parse("#\\space").unwrap();
        parse("#\\newline").unwrap();
    }

    #[test]
    #[ignore = "Complex numbers not supported"]
    fn test_parse_complex_numbers() {
        parse("3+4i").unwrap();
        parse("1/2").unwrap();
    }

    #[test]
    #[ignore = "Vectors not implemented"]
    fn test_parse_vectors() {
        parse("#(1 2 3)").unwrap();
    }

    #[test]
    #[ignore = "Advanced string escapes not supported"]
    fn test_parse_advanced_string_escapes() {
        parse("\"line1\\nline2\"").unwrap();
        parse("\"tab\\there\"").unwrap();
        parse("\"unicode\\u0041\"").unwrap();
    }
}