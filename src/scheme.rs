use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while_m_n, take_while1},
    character::complete::{char, multispace0, multispace1, none_of},
    combinator::{cut, map, map_res, opt, recognize, value},
    error::{ErrorKind, FromExternalError, ParseError as NomParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
};

use crate::MAX_PARSE_DEPTH;
use crate::ast::{NumberType, Value, is_symbol_valid};
use crate::builtinops::{find_scheme_op, get_quote_op};
use crate::{Error, ParseError, ParseErrorKind};

/// Custom nom error type that stores ParseErrorKind and optional dynamic message
#[derive(Debug, Clone, PartialEq)]
struct SchemeParseError<I> {
    input: I,
    nom_kind: ErrorKind,
    parse_kind: Option<ParseErrorKind>, // None means use nom_kind mapping at conversion time
    message: Option<String>,
}

impl<I> SchemeParseError<I> {
    /// Create a new error with a custom message (via closure).
    /// Returns `nom::Err::Error` directly to avoid intermediate wrapping.
    /// The closure is only evaluated on the error path (zero cost in success case).
    fn error<F>(input: I, nom_kind: ErrorKind, parse_kind: ParseErrorKind, f: F) -> nom::Err<Self>
    where
        F: FnOnce() -> String,
    {
        nom::Err::Error(SchemeParseError {
            input,
            nom_kind,
            parse_kind: Some(parse_kind),
            message: Some(f()),
        })
    }
}

impl<I> NomParseError<I> for SchemeParseError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        // Just store nom's ErrorKind. Don't map to ParseErrorKind or create messages yet.
        // That happens in to_parse_error() only if .context() hasn't set them explicitly.
        SchemeParseError {
            input,
            nom_kind: kind,
            parse_kind: None, // Will be mapped from nom_kind if still None at conversion time
            message: None,
        }
    }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<I, E> FromExternalError<I, E> for SchemeParseError<I> {
    fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
        SchemeParseError {
            input,
            nom_kind: kind,
            parse_kind: None, // Will be mapped from nom_kind if still None at conversion time
            message: None,
        }
    }
}

/// Type alias for parser results using our custom error type
/// The 'input lifetime is tied to the input string slice
type SResult<'input, O> = IResult<&'input str, O, SchemeParseError<&'input str>>;

/// Extension trait to add context() method to parser results.
///
/// This allows attaching rich, dynamic error messages using closures that are only
/// evaluated on the error path (no cost in success case).
///
/// **First-wins strategy**: If a message is already set, subsequent context() calls
/// are ignored. This ensures the most specific error (closest to the actual failure)
/// is preserved.
///
/// # Example
/// ```ignore
/// opt(tag("quote")).parse(input).context(|| format!("missing quote at {}", input))?;
/// opt(tag("quote")).parse(input).context_with_kind(ParseErrorKind::ImplementationLimit, || format!("too complex"))?;
/// ```
trait ResultExt<T> {
    /// Attach context with default ParseErrorKind::InvalidSyntax
    fn context<F>(self, f: F) -> Self
    where
        F: FnOnce() -> String;

    /// Attach context with explicit ParseErrorKind
    #[expect(dead_code)]
    fn context_with_kind<F>(self, kind: ParseErrorKind, f: F) -> Self
    where
        F: FnOnce() -> String;
}

impl<'input, T> ResultExt<T> for SResult<'input, T> {
    fn context<F>(self, f: F) -> Self
    where
        F: FnOnce() -> String,
    {
        self.map_err(|e| match e {
            nom::Err::Error(err) => {
                // Only set message and parse_kind if not already set (first-wins)
                if err.message.is_none() {
                    nom::Err::Error(SchemeParseError {
                        parse_kind: err.parse_kind.or(Some(ParseErrorKind::InvalidSyntax)), // Use existing or default to InvalidSyntax
                        message: Some(f()),
                        ..err
                    })
                } else {
                    nom::Err::Error(err)
                }
            }
            nom::Err::Failure(err) => {
                // Only set message and parse_kind if not already set (first-wins)
                if err.message.is_none() {
                    nom::Err::Failure(SchemeParseError {
                        parse_kind: err.parse_kind.or(Some(ParseErrorKind::InvalidSyntax)), // Use existing or default to InvalidSyntax
                        message: Some(f()),
                        ..err
                    })
                } else {
                    nom::Err::Failure(err)
                }
            }
            nom::Err::Incomplete(n) => nom::Err::Incomplete(n),
        })
    }

    fn context_with_kind<F>(self, kind: ParseErrorKind, f: F) -> Self
    where
        F: FnOnce() -> String,
    {
        self.map_err(|e| match e {
            nom::Err::Error(err) => {
                // Set kind if not already set, but use first-wins for message
                nom::Err::Error(SchemeParseError {
                    parse_kind: err.parse_kind.or(Some(kind)), // Use existing or provided kind
                    message: err.message.or_else(|| Some(f())), // Use existing or new message
                    ..err
                })
            }
            nom::Err::Failure(err) => {
                // Set kind if not already set, but use first-wins for message
                nom::Err::Failure(SchemeParseError {
                    parse_kind: err.parse_kind.or(Some(kind)), // Use existing or provided kind
                    message: err.message.or_else(|| Some(f())), // Use existing or new message
                    ..err
                })
            }
            nom::Err::Incomplete(n) => nom::Err::Incomplete(n),
        })
    }
}

/// Configuration for the Scheme parser.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ParseConfig {
    /// Script syntax mode - enables comments and multiline escapes suitable for scripts (not 1-liners)
    ///
    /// When true, enables:
    /// - Single-line (;) and multi-line (#|...|#) comments
    /// - Line continuations in strings: `\<ws>*<newline><ws>*`
    /// - Line continuations between tokens: `\<ws>*<newline><ws>*`
    ///
    /// When false, these features are rejected as parse errors.
    /// Default: false (strict subset mode for 1-line expressions)
    pub script_syntax: bool,
}

/// Helper function to create a quote PrecompiledOp
fn create_quote_precompiled_op(content: &Value) -> Value {
    let builtin_op = get_quote_op();
    Value::PrecompiledOp {
        op: builtin_op,
        op_id: builtin_op.scheme_id.into(),
        args: vec![content.clone()],
    }
}

/// Control whether builtin operations should be precompiled during parsing
#[derive(Debug, Clone, Copy, PartialEq)]
enum ShouldPrecompileOps {
    Yes,
    No,
}

/// Convert nom parsing errors to our structured `ParseError` with context.
///
/// This performs lazy mapping from nom ErrorKind to ParseErrorKind when not explicitly set.
/// The strategy is:
/// - If parse_kind is Some, use it (explicit wins)
/// - If parse_kind is None, map from nom_kind to an appropriate ParseErrorKind
/// - If message is Some, use it (explicit wins)
/// - If message is None, generate a default message based on nom_kind
fn to_parse_error(input: &str, e: nom::Err<SchemeParseError<&str>>) -> Error {
    let (error_input, parse_kind_opt, nom_kind, custom_message) = match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => (e.input, e.parse_kind, e.nom_kind, e.message),
        nom::Err::Incomplete(_) => (
            "",
            Some(ParseErrorKind::Incomplete),
            nom::error::ErrorKind::Eof, // Doesn't matter, parse_kind is set
            Some("Incomplete input".to_string()),
        ),
    };

    // Map nom ErrorKind to ParseErrorKind if not explicitly set
    let parse_kind = parse_kind_opt.unwrap_or(match nom_kind {
        nom::error::ErrorKind::TooLarge => ParseErrorKind::ImplementationLimit,
        nom::error::ErrorKind::Eof | nom::error::ErrorKind::Complete => ParseErrorKind::Incomplete,
        _ => ParseErrorKind::InvalidSyntax,
    });

    // Generate default message from nom ErrorKind if not explicitly set
    let message = custom_message.unwrap_or_else(|| match nom_kind {
        nom::error::ErrorKind::Tag => "Expected specific tag".to_string(),
        nom::error::ErrorKind::Char => "Expected specific character".to_string(),
        nom::error::ErrorKind::OneOf => "Expected one of a set of characters".to_string(),
        nom::error::ErrorKind::NoneOf => "Unexpected character from excluded set".to_string(),
        nom::error::ErrorKind::Eof => "Unexpected end of input".to_string(),
        nom::error::ErrorKind::Alpha => "Expected alphabetic character".to_string(),
        nom::error::ErrorKind::Digit => "Expected digit".to_string(),
        nom::error::ErrorKind::AlphaNumeric => "Expected alphanumeric character".to_string(),
        nom::error::ErrorKind::Space => "Expected whitespace".to_string(),
        nom::error::ErrorKind::TooLarge => "Value too large".to_string(),
        nom::error::ErrorKind::Complete => "Parser requires more data".to_string(),
        nom::error::ErrorKind::TakeWhile1 => "Expected at least one matching character".to_string(),
        nom::error::ErrorKind::Many1 => "Expected at least one match".to_string(),
        nom::error::ErrorKind::Alt => "No alternative matched".to_string(),
        nom::error::ErrorKind::Verify => "Verification failed".to_string(),
        nom::error::ErrorKind::Not => "Unexpected match".to_string(),
        _ => format!("Parse error: {nom_kind:?}"),
    });

    let error_offset = input.len() - error_input.len();

    // Extract the first few characters at the error position as the "found" token
    let found = if !error_input.is_empty() {
        let found_str: String = error_input.chars().take(10).collect();
        Some(if found_str.len() < error_input.len() {
            format!("{found_str}...")
        } else {
            found_str
        })
    } else {
        Some("end of input".to_string())
    };

    Error::ParseError(ParseError::with_context_and_found(
        parse_kind,
        message,
        input,
        error_offset,
        found,
    ))
}

/// Parse a number (integer only, supports decimal and hexadecimal)
/// R7RS-RESTRICTED: This parser only supports 32-bit integers. It does not support
/// the full R7RS numeric tower, which includes rationals, floating-point numbers,
/// and complex numbers.
fn parse_number(input: &str) -> SResult<'_, Value> {
    alt((parse_hexadecimal, parse_decimal)).parse(input)
}

/// Parse a decimal number
/// R7RS-RESTRICTED: Does not support binary (#b), octal (#o), or scientific notation (1e10)
fn parse_decimal(input: &str) -> SResult<'_, Value> {
    // Parse sign and digits, capturing the matched string
    let (input, number_str) = recognize(pair(
        opt(alt((char('+'), char('-')))),
        take_while1(|c: char| c.is_ascii_digit()),
    ))
    .parse(input)?;

    // After successfully parsing sign + digit(s), use cut to commit
    // This ensures things like "-1e5" give clear errors rather than backtracking to symbol
    cut(|input| match number_str.parse::<NumberType>() {
        Ok(n) => Ok((input, Value::Number(n))),
        Err(_) => {
            // Parse failed - could be due to overflow or invalid format
            Err(SchemeParseError::error(
                input,
                ErrorKind::Digit,
                ParseErrorKind::ImplementationLimit,
                || format!("Decimal integer overflow or invalid format: {number_str}"),
            ))
        }
    })
    .parse(input)
}

/// Parse a hexadecimal number (#x or #X prefix)
/// R7RS-RESTRICTED: Only supports hexadecimal integers, not floating-point hex literals
/// Note: Supports negative hex literals like -#xff and positive like +#xff
fn parse_hexadecimal(input: &str) -> SResult<'_, Value> {
    let (input, sign) = opt(alt((char('+'), char('-')))).parse(input)?;

    preceded(
        alt((tag("#x"), tag("#X"))),
        cut(|input| {
            let (input, hex_digits) = take_while1(|c: char| c.is_ascii_hexdigit())
                .parse(input)
                .context(|| "Expected hexadecimal digit sequence".to_string())?;
            match NumberType::from_str_radix(hex_digits, 16) {
                Ok(n) => {
                    let result =
                        if sign == Some('-') {
                            n.checked_neg().ok_or_else(|| SchemeParseError::error(
                            input,
                            ErrorKind::HexDigit,
                            ParseErrorKind::ImplementationLimit,
                            || format!("Hexadecimal integer overflow on negation: -{hex_digits}"),
                        ))?
                        } else {
                            n
                        };
                    Ok((input, Value::Number(result)))
                }
                Err(_) => Err(SchemeParseError::error(
                    input,
                    ErrorKind::HexDigit,
                    ParseErrorKind::ImplementationLimit,
                    || format!("Hexadecimal integer overflow: {hex_digits}"),
                )),
            }
        }),
    )
    .parse(input)
}

/// Parse a boolean (#t, #f, #true, #false)
/// R7RS-RESTRICTED: This parser only supports the exact lowercase forms `#t`, `#f`,
/// `#true`, and `#false`. The R7RS standard requires these to be case-insensitive.
fn parse_bool(input: &str) -> SResult<'_, Value> {
    alt((
        // Parse longer forms first to avoid partial matches
        value(Value::Bool(true), tag("#true")),
        value(Value::Bool(false), tag("#false")),
        value(Value::Bool(true), tag("#t")),
        value(Value::Bool(false), tag("#f")),
    ))
    .parse(input)
}

/// R7RS-compliant symbol parser.
/// This function implements the full R7RS grammar for identifiers, including
/// peculiar identifiers like `+`, `-`, `...`, and the special `->` prefix.
/// It relies on the `is_symbol_valid` function to perform the validation.
fn parse_symbol(input: &str) -> SResult<'_, Value> {
    if is_symbol_valid(input) {
        Ok(("", Value::Symbol(input.to_string())))
    } else {
        Err(nom::Err::Error(SchemeParseError::from_error_kind(
            input,
            nom::error::ErrorKind::Char,
        )))
    }
}

/// Parse a string literal using nom combinators with full R7RS escape sequence support.
/// When script_syntax is enabled, supports line continuations: \<ws>*<newline><ws>*
fn parse_string(config: ParseConfig) -> impl FnMut(&str) -> SResult<Value> {
    move |input: &str| {
        // This is much more performant than may first appear.
        // We are handling Option<char> to give us the ability to skip escaped newlines, but the Rust
        // compiler implements niche optimization on Option<char> by representing None as an invalid
        // value for char - 0x00110000 - so this is exactly the same as using char + sentinel.
        delimited(
        char('"'),
        map(
            many0(alt((
                // Handle any character that is not a quote or backslash
                map(none_of("\"\\"), Some),
                // Handle an escape sequence
                preceded(
                    char('\\'),
                    // cut commits the parser; if this fails, the whole string parse fails
                    cut(alt((
                        // Simple escape sequences
                        value(Some('\\'), char('\\')),
                        value(Some('"'), char('"')),
                        value(Some('\n'), char('n')),
                        value(Some('\t'), char('t')),
                        value(Some('\r'), char('r')),
                        value(Some('\u{07}'), char('a')), // alarm (bell)
                        value(Some('\u{08}'), char('b')), // backspace
                        // Hexadecimal escape: \x<hex digits>;
                        map(
                            map_res(
                                preceded(
                                    char('x'),
                                    cut(terminated(
                                        take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()),
                                        char(';'),
                                    )),
                                ),
                                |hex_str: &str| {
                                    u32::from_str_radix(hex_str, 16)
                                        .map_err(|_| "Invalid hex number")
                                        // Note that char::from_u32 validates against invalid Unicode
                                        // like unpaired surrogates or characters out of range
                                        .and_then(|code| {
                                            char::from_u32(code).ok_or("Invalid Unicode")
                                        })
                                },
                            ),
                            Some,
                        ),
                        // Line continuation: backslash, optional whitespace, newline, optional whitespace
                        // R7RS pattern: \<intraline whitespace>*<line ending><intraline whitespace>*
                        // Only available when script_syntax is enabled
                        // Returns None to indicate this should be filtered out
                        |input_inner| {
                            // Try to match line continuation pattern: \<intraline ws>*<line ending><intraline ws>*
                            let line_cont_result: SResult<&str> = preceded(
                                take_while::<_, _, SchemeParseError<_>>(|c: char| c == ' ' || c == '\t'),
                                preceded(
                                    alt((tag::<_, _, SchemeParseError<_>>("\n"), tag("\r\n"))),
                                    take_while::<_, _, SchemeParseError<_>>(|c: char| c == ' ' || c == '\t'),
                                ),
                            )
                            .parse(input_inner);

                            match line_cont_result {
                                Ok((remaining, _)) if config.script_syntax => {
                                    // Line continuation pattern matched and script_syntax enabled
                                    Ok((remaining, None))
                                }
                                Ok((_, _)) => {
                                    // Line continuation pattern matched but script_syntax disabled
                                    Err(SchemeParseError::error(
                                        input_inner,
                                        ErrorKind::Char,
                                        ParseErrorKind::Unsupported,
                                        || "Line continuations in strings require script_syntax mode".to_string(),
                                    ))
                                }
                                Err(_) => {
                                    // Not a line continuation pattern - unknown escape sequence
                                    Err(SchemeParseError::error(
                                        input_inner,
                                        ErrorKind::Alt,
                                        ParseErrorKind::InvalidSyntax,
                                        || "Unknown escape sequence".to_string(),
                                    ))
                                }
                            }
                        },
                    ))),
                ),
            ))),
            |char_options| {
                // Filter out None values (line continuations) and collect remaining chars
                let filtered: String = char_options.into_iter().flatten().collect();
                Value::String(filtered)
            },
        ),
        char('"'),
    )
    .parse(input)
    }
}

/// A combinator that consumes whitespace and, if enabled, comments.
/// Uses nom's multispace0/multispace1 which handle Unicode whitespace via char::is_whitespace()
/// Also handles R7RS line continuations: \<intraline whitespace>*<line ending><intraline whitespace>*
fn whitespace_or_comment(config: ParseConfig) -> impl for<'a> FnMut(&'a str) -> SResult<()> {
    move |input: &str| {
        let (mut input, _) = multispace0(input)?;

        loop {
            let (next_input, _) = multispace0(input)?;
            input = next_input;

            // Try to match comment or line continuation
            let comment_result = alt((
                // Single-line comment: ; ... until newline
                // R7RS-RESTRICTED: Does not support S-expression comments #;(expr)
                value('s', pair(char(';'), is_not("\n\r"))),
                // Multi-line comment: #| ... |# (handles nesting)
                value('b', block_comment),
                // Line continuation: \<intraline whitespace>*<line ending><intraline whitespace>*
                // Allows breaking long lines outside of strings
                value(
                    'l',
                    preceded(
                        char('\\'),
                        preceded(
                            take_while(|c: char| c == ' ' || c == '\t'),
                            preceded(
                                alt((tag("\n"), tag("\r\n"))),
                                take_while(|c: char| c == ' ' || c == '\t'),
                            ),
                        ),
                    ),
                ),
            ))
            .parse(input);

            match comment_result {
                Ok((next_input, _)) if config.script_syntax => {
                    // Comment or line continuation found and script_syntax enabled
                    input = next_input;
                }
                Ok((_, marker)) => {
                    // Comment or line continuation found but script_syntax disabled
                    let feature = match marker {
                        's' => "Single-line comments (;)",
                        'b' => "Block comments (#|...|#)",
                        'l' => "Line continuations (\\<newline>)",
                        _ => "Script syntax features",
                    };
                    return Err(nom::Err::Error(SchemeParseError {
                        input,
                        nom_kind: ErrorKind::Char,
                        parse_kind: Some(ParseErrorKind::Unsupported),
                        message: Some(format!("{feature} require script_syntax mode")),
                    }));
                }
                Err(_) => {
                    // No comment or line continuation found
                    break;
                }
            }
        }

        Ok((input, ()))
    }
}

/// Parses a nested block comment, as defined in R7RS.
fn block_comment(input: &str) -> SResult<'_, ()> {
    let (input, _) = preceded(tag("#|"), cut(|input| Ok((input, ()))))
        .parse(input)
        .context(|| "Expected block comment after '#|'".to_string())?;
    let mut depth = 1;
    let mut current_input = input;

    while depth > 0 {
        let (next_input, block_char) =
            alt((tag("|#"), tag("#|"), take_while1(|c| c != '#' && c != '|')))
                .parse(current_input)?;
        match block_char {
            "|#" => depth -= 1,
            "#|" => depth += 1,
            _ => {}
        }
        current_input = next_input;
    }

    Ok((current_input, ()))
}

/// Parse a list with configurable precompilation behavior (performance optimized)
/// R7RS-RESTRICTED: This parser only supports proper lists. It does not support
/// improper lists or dotted pairs (e.g., `(1 . 2)`).
fn parse_list(
    input: &str,
    should_precompile: ShouldPrecompileOps,
    config: ParseConfig,
    depth: usize,
) -> SResult<'_, Value> {
    // After the opening '(', we cut to prevent backtracking.
    // An open parenthesis unambiguously starts a list.
    let (input, _) = preceded(char('('), cut(whitespace_or_comment(config)))
        .parse(input)
        .context(|| "Expected list after '('".to_string())?;

    // Early quote detection to avoid backtracking
    let (input, is_quote) = opt(tag("quote")).parse(input)?;

    if is_quote.is_some() {
        // Handle quote specially - parse exactly one more element unprecompiled
        let (input, _) = cut(multispace1)
            .parse(input)
            .context(|| "Expected space after 'quote'".to_string())?;
        let (input, content) =
            cut(|input| parse_sexpr(input, ShouldPrecompileOps::No, config, depth + 1))
                .parse(input)
                .context(|| "Expected expression after 'quote'".to_string())?;
        let (input, _) = whitespace_or_comment(config)(input)?;
        let (input, _) = cut(char(')'))
            .parse(input)
            .context(|| "Expected closing ')' after quoted expression".to_string())?;

        // If precompilation is enabled, create a PrecompiledOp
        if should_precompile == ShouldPrecompileOps::Yes {
            let precompiled = create_quote_precompiled_op(&content);
            return Ok((input, precompiled));
        }

        // Fallback to unprecompiled list representation (only when precompilation disabled)
        return Ok((
            input,
            Value::List(vec![Value::Symbol("quote".into()), content]),
        ));
    }

    // Regular list parsing - parse all elements in one pass
    let (input, elements) = cut(separated_list0(multispace1, |input| {
        parse_sexpr(input, should_precompile, config, depth + 1)
    }))
    .parse(input)
    .context(|| "Expected list elements".to_string())?;

    // Parse closing parenthesis and whitespace
    let (input, _) = whitespace_or_comment(config)(input)?;
    let (input, _) = cut(char(')'))
        .parse(input)
        .context(|| "Expected closing ')'".to_string())?;

    // Apply precompilation if enabled - single lookup, no repeated string comparison
    if should_precompile == ShouldPrecompileOps::Yes
        && let [Value::Symbol(op_name), args @ ..] = elements.as_slice()
        && let Some(builtin_op) = find_scheme_op(op_name.as_str())
    {
        return Ok((
            input,
            Value::PrecompiledOp {
                op: builtin_op,
                op_id: builtin_op.scheme_id.into(),
                args: args.to_vec(),
            },
        ));
    }

    Ok((input, Value::List(elements)))
}

/// This is the new two-phase atom parser.
/// 1. Lex phase: It first consumes a complete "atom" token from the input stream.
///    An atom is a sequence of characters that is not a list, string, or comment.
///    It reads until it hits whitespace or a delimiter like '(', ')', or '"'.
/// 2. Parse phase: It then attempts to parse the collected token as a specific type.
///    It tries `parse_bool` and `parse_number` first. Since these can fail if the token
///    is not a valid boolean or number, it uses `parse_symbol` as a fallback.
///
/// This approach solves ambiguity problems like "123var", ensuring the entire token
/// is considered, which results in a more accurate `InvalidSyntax` error instead of
/// a misleading `TrailingContent` error.
fn parse_atom(input: &str) -> SResult<'_, Value> {
    // Phase 1: Lex the atom.
    // An atom is terminated by whitespace or delimiters. Note that prefixes like '#'
    // are part of the atom itself (e.g., #t, #x1A), so they are not delimiters here.
    let (remaining, atom_str) = recognize(take_while1(|c: char| {
        !c.is_whitespace() && !matches!(c, '(' | ')' | '"')
    }))
    .parse(input)?;

    // Phase 2: Parse the lexed atom string.
    // Each parser must consume the *entire* atom. `terminated(..., eof)` enforces this.
    use nom::combinator::eof;
    let result = alt((
        // Attempt to parse the most specific tokens first.
        terminated(parse_bool, eof),
        terminated(parse_number, eof),
        // `parse_symbol` is the fallback for any other valid atom.
        terminated(parse_symbol, eof),
    ))
    .parse(atom_str);

    match result {
        Ok((_, value)) => Ok((remaining, value)), // Success, return the value and the original remaining input.
        Err(_) => {
            // If all parsers fail, it's an invalid atom.
            // We create a specific error pointing to the location after the bad atom.
            Err(nom::Err::Failure(SchemeParseError {
                input: remaining,
                nom_kind: ErrorKind::Alt,
                parse_kind: Some(ParseErrorKind::InvalidSyntax),
                message: Some(format!("Invalid atomic expression: '{atom_str}'")),
            }))
        }
    }
}

/// Parse an S-expression with configurable precompilation behavior
/// R7RS-RESTRICTED: This parser does not support several R7RS data types and syntax, including:
/// - Characters (e.g., `#\a`, `#\space`, `#\newline`, `#\x03BB`)
/// - Vectors (e.g., `#(1 2 3)`)
/// - Bytevectors (e.g., `#u8(0 255)`)
/// - Datum labels for shared/circular structures (e.g., `#1=(a #1#)`)
/// - Quasiquote syntax is intentionally unsupported and will fail parsing.
/// - Syntax objects: `#'expr`, `#,expr`, `#,@expr`
/// - Case folding directives: `#!fold-case`, `#!no-fold-case`
/// - Read-time evaluation: `#.(expr)`
fn parse_sexpr(
    input: &str,
    should_precompile: ShouldPrecompileOps,
    config: ParseConfig,
    depth: usize,
) -> SResult<'_, Value> {
    if depth >= MAX_PARSE_DEPTH {
        return Err(nom::Err::Error(SchemeParseError::from_error_kind(
            input,
            nom::error::ErrorKind::TooLarge,
        )));
    }
    preceded(
        whitespace_or_comment(config),
        alt((
            unsupported_forms,
            |input| parse_quote(input, should_precompile, config, depth), // Pass precompilation setting to quote
            |input| parse_list(input, should_precompile, config, depth),
            parse_string(config),
            parse_atom, // Replaced individual atom parsers with the new unified one.
                        // R7RS-RESTRICTED: Missing quasiquote (`), unquote (,), unquote-splicing (,@)
                        // R7RS-RESTRICTED: Missing syntax quote (#'), syntax unquote (#,), syntax unquote-splicing (#,@)
        )),
    )
    .parse(input)
}

/// Helper: Match dot only when standalone or followed by digit (floating point)
/// Does NOT match .foo (valid symbol starting with dot)
fn parse_restricted_dot<'a>(input: &'a str) -> SResult<'a, char> {
    let (remaining, _) = char::<&'a str, SchemeParseError<&'a str>>('.').parse(input)?;
    // Check if followed by digit (floating point like .5) or end-of-term (standalone dot)
    let next_char = remaining.chars().next();
    if remaining.is_empty()
        || next_char
            .map(|c| c.is_ascii_whitespace() || c == ')' || c.is_ascii_digit())
            .unwrap_or(false)
    {
        Ok((remaining, '.'))
    } else {
        // Followed by letter or other symbol char - let parse_symbol handle it
        Err(nom::Err::Error(SchemeParseError {
            input: remaining,
            nom_kind: ErrorKind::Char,
            parse_kind: None,
            message: None,
        }))
    }
}

/// Fails parsing if it encounters any unsupported R7RS forms
/// This parser detects and explicitly rejects language features we don't support
/// R7RS-RESTRICTED: Many R7RS syntax forms are intentionally unsupported
fn unsupported_forms(input: &str) -> SResult<'_, Value> {
    // Try to match various unsupported syntax forms by their unambiguous prefixes
    let result = alt((
        // === Unsupported number formats ===
        // R7RS-RESTRICTED: Only 32-bit decimal and hexadecimal integers are supported
        // Binary (#b), Octal (#o), Exact (#e), Inexact (#i) - with optional +/- sign
        preceded(
            opt(alt((char('+'), char('-')))),
            preceded(
                alt((
                    tag("#b"),
                    tag("#B"), // Binary: #b101010
                    tag("#o"),
                    tag("#O"), // Octal: #o755
                    tag("#e"),
                    tag("#E"), // Exact: #e1.5
                    tag("#i"),
                    tag("#I"), // Inexact: #i2
                )),
                |i| Ok((i, 'n')), // 'n' = unsupported number format
            ),
        ),
        // === Quasiquote/Unquote shorthand ===
        // R7RS-RESTRICTED: Quasiquote (`) and unquote (,) are not supported
        char('`'), // quasiquote
        char(','), // unquote
        // === Dotted pair syntax and floating point: . or .<digit> ===
        // R7RS-RESTRICTED: Improper lists (dotted pairs) and floating point not supported
        // Match '.' only when standalone (followed by whitespace/paren/EOF) or followed by digit (floating point)
        // Do NOT match '.foo' (valid symbol)
        parse_restricted_dot,
        // === Block comments: #| ... |# ===
        // NOTE: Block comments are handled in whitespace_or_comment() where script_syntax mode is validated.
        // No need to check here since they're consumed before unsupported_forms is ever called.
        // === Character literals: #\a, #\space, #\newline, #\x03BB ===
        // R7RS-RESTRICTED: Character literals are not supported
        // The #\ prefix uniquely identifies character literals
        preceded(tag("#\\"), |i| Ok((i, '🤷'))),
        // === Vectors: #(1 2 3) ===
        // R7RS-RESTRICTED: Vectors are not supported
        // The #( prefix uniquely identifies vectors
        preceded(tag("#("), |i| Ok((i, '🤷'))),
        // === Bytevectors: #u8(0 255) ===
        // R7RS-RESTRICTED: Bytevectors are not supported
        // The #u8( prefix uniquely identifies bytevectors
        preceded(tag("#u8("), |i| Ok((i, '🤷'))),
        // === Datum labels: #1=(a #1#) for shared/circular structures ===
        // R7RS-RESTRICTED: Datum labels for shared/circular structures are not supported
        // The #<digit> prefix uniquely identifies datum labels
        preceded(
            char('#'),
            preceded(
                take_while1(|c: char| c.is_ascii_digit()),
                alt((char('='), char('#'))),
            ),
        ),
        // === Syntax objects: #'expr, #,expr, #,@expr ===
        // R7RS-RESTRICTED: Syntax objects are not supported
        preceded(char('#'), alt((char('\''), char(',')))),
        // === Symbols with spaces: |symbol with spaces| ===
        // R7RS-RESTRICTED: Pipe-delimited symbols are not supported
        char('|'),
        // === Directives: #!fold-case, #!no-fold-case ===
        // R7RS-RESTRICTED: Directives are not supported
        preceded(tag("#!"), |i| Ok((i, '🤷'))),
        // === Read-time evaluation: #.(expr) ===
        // R7RS-RESTRICTED: Read-time evaluation is not supported
        preceded(tag("#."), |i| Ok((i, '🤷'))),
        // === S-expression comments: #;(expr) ===
        // R7RS-RESTRICTED: S-expression comments are not supported
        preceded(tag("#;"), |i| Ok((i, '🤷'))),
    ))
    .parse(input);

    match result {
        Ok((remaining, marker)) => {
            // Matched an unsupported form - provide specific error message
            let message = if marker == 'n' {
                // Matched an unsupported number format (#b, #o, #e, #i with optional +/-)
                "Only 32-bit integers in decimal or hexadecimal (#x) notation are supported."
            } else {
                "Unsupported R7RS syntax"
            };

            Err(nom::Err::Failure(SchemeParseError {
                input: remaining,
                nom_kind: ErrorKind::Char,
                parse_kind: Some(ParseErrorKind::Unsupported),
                message: Some(message.to_string()),
            }))
        }
        Err(e) => Err(e), // Didn't match - let alt() try next parser
    }
}

/// Parse quoted expression ('expr -> (quote expr))
fn parse_quote(
    input: &str,
    should_precompile: ShouldPrecompileOps,
    config: ParseConfig,
    depth: usize,
) -> SResult<'_, Value> {
    let (input, _) = char('\'').parse(input)?;
    let (input, expr) = cut(|input| parse_sexpr(input, ShouldPrecompileOps::No, config, depth + 1))
        .parse(input)
        .context(|| "Expected expression after quote".to_string())?; // Use unprecompiled parsing for quoted content

    // Create PrecompiledOp only if precompilation is enabled
    if should_precompile == ShouldPrecompileOps::Yes {
        let precompiled = create_quote_precompiled_op(&expr);
        return Ok((input, precompiled));
    }

    // Fallback to unprecompiled representation (only when precompilation disabled)
    Ok((
        input,
        Value::List(vec![Value::Symbol("quote".into()), expr]),
    ))
}

/// Parse a complete S-expression from input with optimization enabled
pub fn parse_scheme(input: &str) -> Result<Value, Error> {
    parse_scheme_with_config(input, ParseConfig::default())
}

/// Parse a complete S-expression from input with a specific configuration.
pub fn parse_scheme_with_config(input: &str, config: ParseConfig) -> Result<Value, Error> {
    match terminated(
        |input| parse_sexpr(input, ShouldPrecompileOps::Yes, config, 0),
        whitespace_or_comment(config),
    )
    .parse(input)
    {
        Ok(("", value)) => {
            // After successful parsing, validate arity for any PrecompiledOp
            validate_arity_in_ast(&value)?;
            Ok(value)
        }
        Ok((remaining, _)) => {
            let offset = input.len() - remaining.len();
            Err(Error::ParseError(ParseError::with_context(
                ParseErrorKind::TrailingContent,
                "Unexpected remaining input",
                input,
                offset,
            )))
        }
        Err(e) => Err(to_parse_error(input, e)),
    }
}

/// Recursively validate arity in parsed AST - simpler than threading through parser
fn validate_arity_in_ast(value: &Value) -> Result<(), Error> {
    match value {
        Value::PrecompiledOp { op, args, .. } => {
            // Validate this operation's arity
            if let Err(Error::ArityError { expected, got, .. }) = op.validate_arity(args.len()) {
                return Err(Error::arity_error_with_expr(
                    expected,
                    got,
                    format!("{}", value.to_uncompiled_form()),
                ));
            }
            // Recursively validate nested expressions
            for arg in args {
                validate_arity_in_ast(arg)?;
            }
        }
        Value::List(elements) => {
            // Recursively validate list elements
            for element in elements {
                validate_arity_in_ast(element)?;
            }
        }
        _ => {} // Other value types don't need validation
    }
    Ok(())
}

#[cfg(test)]
#[expect(clippy::unwrap_used)] // test code OK
mod tests {
    use super::*;
    use crate::ast::{nil, sym, val};

    /// Test result variants for comprehensive parsing tests
    #[derive(Debug)]
    enum ParseTestResult {
        Success(Value), // Parsing should succeed with this value
        SuccessPrecompiledOp(&'static str, Vec<Value>), // Should succeed with PrecompiledOp(op_id, args)
        SemanticallyEquivalent(Value), // Should succeed and be semantically equivalent (for quote shorthand)
        ParseError(ParseErrorKind),    // Parsing should fail with specific ParseErrorKind
        #[expect(dead_code)] // Reserved for future use when we can test arity errors post-parse
        ArityError, // Parsing should fail with arity validation error
    }
    use ParseErrorKind::*;
    use ParseTestResult::*;

    /// Helper for successful parse test cases
    fn success<T: Into<Value>>(value: T) -> ParseTestResult {
        Success(value.into())
    }

    /// Helper for PrecompiledOp test cases
    fn precompiled_op(op_id: &'static str, args: Vec<Value>) -> ParseTestResult {
        SuccessPrecompiledOp(op_id, args)
    }

    /// Helper for SemanticallyEquivalent test cases (used for quote shorthand)
    fn semantically_equivalent<T: Into<Value>>(value: T) -> ParseTestResult {
        SemanticallyEquivalent(value.into())
    }

    /// Run comprehensive parse tests with simplified error reporting and round-trip validation
    fn run_parse_tests(
        test_cases: Vec<(&str, ParseTestResult)>,
        config: ParseConfig,
        test_suite_name: &str,
    ) {
        for (i, (input, expected)) in test_cases.iter().enumerate() {
            let test_id = format!("[{}] Test #{}", test_suite_name, i + 1);
            let result = parse_scheme_with_config(input, config);

            match (result, expected) {
                // Success cases with round-trip testing
                (Ok(actual), Success(expected_val)) => {
                    assert_eq!(
                        actual, *expected_val,
                        "{test_id}: value mismatch for input '{input}'"
                    );

                    // Test round-trip: display -> parse -> display should be identical
                    let displayed = format!("{actual}");
                    let reparsed_result = parse_scheme_with_config(&displayed, config);
                    match reparsed_result {
                        Ok(reparsed) => {
                            let redisplayed = format!("{reparsed}");
                            assert_eq!(
                                displayed, redisplayed,
                                "{test_id}: round-trip display mismatch for '{input}'"
                            );
                        }
                        Err(e) => {
                            panic!("{test_id}: round-trip parse failed for '{displayed}': {e:?}")
                        }
                    }
                }

                (Ok(actual), SuccessPrecompiledOp(expected_op_id, expected_args)) => {
                    if let Value::PrecompiledOp { op_id, args, .. } = &actual {
                        assert_eq!(
                            op_id, expected_op_id,
                            "{test_id}: op_id mismatch for input '{input}'"
                        );
                        assert_eq!(
                            args, expected_args,
                            "{test_id}: args mismatch for input '{input}'"
                        );

                        // Test round-trip for PrecompiledOp
                        let displayed = format!("{actual}");
                        let reparsed =
                            parse_scheme_with_config(&displayed, config).unwrap_or_else(|e| {
                                panic!(
                                    "{test_id}: round-trip parse failed for '{displayed}': {e:?}"
                                )
                            });
                        let redisplayed = format!("{reparsed}");
                        assert_eq!(
                            displayed, redisplayed,
                            "{test_id}: round-trip display mismatch for '{input}'"
                        );
                    } else {
                        panic!(
                            "{test_id}: expected PrecompiledOp for input '{input}', got {actual:?}"
                        );
                    }
                }

                (Ok(actual), SemanticallyEquivalent(expected_val)) => {
                    // For semantically equivalent cases, we compare the uncompiled forms
                    // This is useful for quote shorthand ('foo vs (quote foo))
                    let actual_uncompiled = actual.to_uncompiled_form();
                    assert_eq!(
                        actual_uncompiled, *expected_val,
                        "{test_id}: semantic equivalence mismatch for input '{input}'"
                    );

                    // Test round-trip for SemanticallyEquivalent
                    let displayed = format!("{actual}");
                    let reparsed =
                        parse_scheme_with_config(&displayed, config).unwrap_or_else(|e| {
                            panic!("{test_id}: round-trip parse failed for '{displayed}': {e:?}")
                        });
                    let redisplayed = format!("{reparsed}");
                    assert_eq!(
                        displayed, redisplayed,
                        "{test_id}: round-trip display mismatch for '{input}'"
                    );
                }

                // Error cases - successful matches
                (Err(Error::ParseError(parse_err)), ParseError(expected_kind)) => {
                    assert_eq!(
                        parse_err.kind, *expected_kind,
                        "{test_id} input '{input}': expected ParseErrorKind::{expected_kind:?}, got {:?}",
                        parse_err.kind
                    );
                }
                (Err(Error::ArityError { .. }), ArityError) => {} // Arity error

                // Success when error expected
                (Ok(actual), ParseError(kind)) => {
                    panic!(
                        "{test_id} input '{input}': expected ParseError({kind:?}), got success: {actual:?}"
                    );
                }
                (Ok(actual), ArityError) => {
                    panic!(
                        "{test_id} input '{input}': expected ArityError, got success: {actual:?}"
                    );
                }

                // Error when success expected
                (Err(err), Success(_))
                | (Err(err), SuccessPrecompiledOp(_, _))
                | (Err(err), SemanticallyEquivalent(_)) => {
                    panic!("{test_id} input '{input}': expected success, got error {err:?}");
                }

                // Wrong error type mismatches
                (Err(Error::ParseError(parse_err)), ArityError) => {
                    panic!(
                        "{test_id} input '{input}': expected ArityError, got ParseError: {parse_err:?}"
                    );
                }
                (Err(Error::ArityError { .. }), ParseError(kind)) => {
                    panic!(
                        "{test_id} input '{input}': expected ParseError({kind:?}), got ArityError"
                    );
                }

                // Catch-all for unexpected error types (EvalError, TypeError, UnboundVariable in tests)
                (Err(err), ParseError(_) | ArityError) => {
                    panic!("{test_id} input '{input}': unexpected error type: {err:?}");
                }
            }
        }
    }

    #[test]
    fn test_arity_validation() {
        // These tests check the arity validation that happens *after* a successful parse.
        // Note: quote is special-cased in parsing and validates arity there,
        // so we can't test it here. Most other operations validate during parse too.
        let arity_tests = vec![
            // Valid arity
            ("(+ 1 2)", precompiled_op("+", vec![val(1), val(2)])),
            ("(+ 1)", precompiled_op("+", vec![val(1)])),
        ];

        run_parse_tests(arity_tests, ParseConfig::default(), "ArityValidation");
    }

    #[test]
    #[expect(clippy::too_many_lines)] // Comprehensive test coverage is intentionally thorough
    fn test_scheme_parser() {
        let test_cases = vec![
            // ===== Numbers (Integers) =====
            ("42", success(42)),
            ("-5", success(-5)),
            ("0", success(0)),
            ("-0", success(0)),
            ("2147483647", success(i32::MAX)),
            ("-2147483648", success(i32::MIN)),
            // ===== Numbers (Hexadecimal) =====
            ("#x1A", success(26)),
            ("#X1a", success(26)), // R7RS requires case-insensitivity for hex digits
            ("#XFF", success(255)), // R7RS requires case-insensitivity for 'x' prefix
            ("#xff", success(255)),
            ("#x0", success(0)),
            ("#x12345", success(74565)),
            ("-#x10", success(-16)),
            ("+#x10", success(16)),
            // ===== Booleans =====
            ("#t", success(true)),
            ("#f", success(false)),
            ("#true", success(true)),
            ("#false", success(false)),
            // ===== Symbols =====
            // R7RS valid symbols
            ("foo", success(sym("foo"))),
            ("test-name", success(sym("test-name"))),
            ("test!", success(sym("test!"))),
            ("test?", success(sym("test?"))),
            ("test_name", success(sym("test_name"))),
            ("test$", success(sym("test$"))),
            ("test%", success(sym("test%"))),
            ("test&", success(sym("test&"))),
            ("test*", success(sym("test*"))),
            ("test/", success(sym("test/"))),
            ("test:", success(sym("test:"))),
            ("test<", success(sym("test<"))),
            ("test=", success(sym("test="))),
            ("test>", success(sym("test>"))),
            ("test^", success(sym("test^"))),
            ("test~", success(sym("test~"))),
            ("a.b", success(sym("a.b"))),
            ("a+b", success(sym("a+b"))),
            ("a-b", success(sym("a-b"))),
            ("a@b", success(sym("a@b"))),
            ("->", success(sym("->"))),
            ("->string", success(sym("->string"))),
            // Peculiar identifiers
            ("+", success(sym("+"))),
            ("-", success(sym("-"))),
            ("...", success(sym("..."))),
            // ===== Symbol Parsing Failures =====
            // R7RS invalid symbols
            ("@foo", ParseError(InvalidSyntax)), // `@` is not an initial character
            (".1", ParseError(Unsupported)),     // This is a number, not a symbol
            (".a", ParseError(InvalidSyntax)),   // `.` is not an initial character
            ("[", ParseError(InvalidSyntax)),
            ("]", ParseError(InvalidSyntax)),
            ("{", ParseError(InvalidSyntax)),
            ("}", ParseError(InvalidSyntax)),
            // ===== Strings =====
            ("\"hello\"", success("hello")),
            ("\"hello world\"", success("hello world")),
            ("\"\"", success("")),
            // Standard escape sequences
            (r#""hello\nworld""#, success("hello\nworld")),
            (r#""quote\"test""#, success("quote\"test")),
            (r#""backslash\\test""#, success("backslash\\test")),
            (r#""tab\here""#, ParseError(InvalidSyntax)), // \h is not a valid escape
            // R7RS-RESTRICTED: Does not support line continuations in strings without script_syntax
            (
                r#""hello \
 world""#,
                ParseError(Unsupported),
            ),
            // R7RS-RESTRICTED: Test boundary of hex escapes
            (
                r#""max_unicode\x10FFFF;""#,
                success("max_unicode\u{10FFFF}"),
            ),
            // ===== Nil (Empty List) =====
            ("()", success(nil())),
            ("( )", success(nil())),
            ("(\t\n)", success(nil())),
            // ===== Lists =====
            ("(1 2 3)", success([1, 2, 3])),
            ("(42)", success([42])),
            (
                "(1 hello \"world\" #t)",
                success([val(1), sym("hello"), val("world"), val(true)]),
            ),
            ("(foo 1 2)", success([sym("foo"), val(1), val(2)])),
            ("(a b c)", success([sym("a"), sym("b"), sym("c")])),
            (
                "(42 is the answer)",
                success([val(42), sym("is"), sym("the"), sym("answer")]),
            ),
            ("((1 2) (3 4))", success([[1, 2], [3, 4]])),
            ("(((1)))", success([val([val([val(1)])])])),
            (
                "(foo (\"bar\" #t) -123)",
                success([sym("foo"), val([val("bar"), val(true)]), val(-123)]),
            ),
            // ===== Precompiled Operations =====
            ("(+ 1 2)", precompiled_op("+", vec![val(1), val(2)])),
            (
                "(* 3 4 5)",
                precompiled_op("*", vec![val(3), val(4), val(5)]),
            ),
            ("(< 1 2)", precompiled_op("<", vec![val(1), val(2)])),
            (
                "(if #t 1 2)",
                precompiled_op("if", vec![val(true), val(1), val(2)]),
            ),
            // Nested precompiled ops
            (
                "(car (list 1 2 3))",
                precompiled_op(
                    "car",
                    vec![val(Value::PrecompiledOp {
                        op: find_scheme_op("list").unwrap(),
                        op_id: "list".into(),
                        args: vec![val(1), val(2), val(3)],
                    })],
                ),
            ),
            // Mixed precompiled and regular lists
            (
                "((+ 1 2) (foo bar))",
                success([
                    val(Value::PrecompiledOp {
                        op: find_scheme_op("+").unwrap(),
                        op_id: "+".into(),
                        args: vec![val(1), val(2)],
                    }),
                    val([sym("foo"), sym("bar")]),
                ]),
            ),
            // ===== Quote Syntax =====
            // Shorthand quote
            (
                "'foo",
                semantically_equivalent(val([sym("quote"), sym("foo")])),
            ),
            (
                "'(1 2 3)",
                semantically_equivalent(val([sym("quote"), val([1, 2, 3])])),
            ),
            ("'()", semantically_equivalent(val([sym("quote"), nil()]))),
            // Longhand quote (precompiled)
            ("(quote foo)", precompiled_op("quote", vec![sym("foo")])),
            (
                "(quote (1 2 3))",
                precompiled_op("quote", vec![val([1, 2, 3])]),
            ),
            // ===== Whitespace =====
            ("  42  ", success(42)),
            ("\t#t\n", success(true)),
            ("\r\n  foo  \t", success(sym("foo"))),
            ("( 1   2\t\n3 )", success([1, 2, 3])),
            // R7RS-RESTRICTED: Test non-standard whitespace characters
            ("(1\x0B2)", ParseError(InvalidSyntax)), // Vertical Tab - not recognized as whitespace
            ("(1\x0C2)", ParseError(InvalidSyntax)), // Form Feed - not recognized as whitespace
            // ===== General Error Handling =====
            // Empty/whitespace-only input
            ("", ParseError(InvalidSyntax)),
            ("   ", ParseError(InvalidSyntax)),
            // Invalid starting characters
            (")", ParseError(InvalidSyntax)),
            ("#", ParseError(InvalidSyntax)), // '#' by itself is invalid
            // Mismatched parentheses
            ("(1 2 3", ParseError(InvalidSyntax)), // Unclosed list
            ("((1 2)", ParseError(InvalidSyntax)), // Unclosed nested list
            ("1 2 3)", ParseError(TrailingContent)), // Parses "1", rest is trailing
            // Multiple expressions
            ("1 2", ParseError(TrailingContent)),
            ("(+ 1 2) (+ 3 4)", ParseError(TrailingContent)),
            ("\"hello\" world", ParseError(TrailingContent)),
            ("#t #f", ParseError(TrailingContent)),
            // ===== Atom Parsing Error Cases (Corrected for parse_atom) =====
            // These now correctly fail as InvalidSyntax for the whole token
            ("123var", ParseError(InvalidSyntax)),
            ("-42name", ParseError(InvalidSyntax)),
            ("123a", ParseError(InvalidSyntax)),
            ("123#", ParseError(InvalidSyntax)),
            ("abc#", ParseError(InvalidSyntax)),
            // These correctly fail as TrailingContent because the delimiter is not part of the atom
            ("123(", ParseError(TrailingContent)),
            ("123)", ParseError(TrailingContent)),
            ("123\"", ParseError(TrailingContent)),
            ("abc(", ParseError(TrailingContent)),
            ("abc)", ParseError(TrailingContent)),
            ("abc\"", ParseError(TrailingContent)),
            ("test#tag", ParseError(InvalidSyntax)), // Parses "test", "#tag" is trailing
            ("test space", ParseError(TrailingContent)), // Parses "test", " space" is trailing
            // ===== Number Parsing Failures =====
            ("99999999999999999999", ParseError(InvalidSyntax)),
            ("-99999999999999999999", ParseError(InvalidSyntax)),
            ("#xG", ParseError(InvalidSyntax)),
            ("#x", ParseError(InvalidSyntax)),
            ("#y123", ParseError(InvalidSyntax)),
            // ===== Boolean Parsing Failures (Case-sensitive) =====
            ("#T", ParseError(InvalidSyntax)),
            ("#F", ParseError(InvalidSyntax)),
            ("#True", ParseError(InvalidSyntax)),
            ("#False", ParseError(InvalidSyntax)),
            ("#TRUE", ParseError(InvalidSyntax)),
            ("#FALSE", ParseError(InvalidSyntax)),
            // ===== String Parsing Failures =====
            (r#""unterminated"#, ParseError(InvalidSyntax)),
            (r#""unterminated\""#, ParseError(InvalidSyntax)), // ends with backslash
            (r#""test\""#, ParseError(InvalidSyntax)),
            (r#""other\zchar""#, ParseError(InvalidSyntax)), // Unknown escape
            (r#""badhex\xZ;""#, ParseError(InvalidSyntax)),  // Invalid hex digit
            (r#""no-semicolon\x41""#, ParseError(InvalidSyntax)), // Missing semicolon
            (r#""empty_hex\x;""#, ParseError(InvalidSyntax)), // Empty hex escape
            // R7RS DEVIATION: The spec says unknown escapes should be the char itself. We fail.
            (r#""unknown_escape\z""#, ParseError(InvalidSyntax)),
            // ===== List/Quote Failures =====
            ("(", ParseError(InvalidSyntax)),
            ("((", ParseError(InvalidSyntax)),
            ("'", ParseError(InvalidSyntax)),
            ("'(1 2", ParseError(InvalidSyntax)), // Dangling quote with list
            // ===== R7RS Unsupported Features =====
            // R7RS-RESTRICTED: No floating point numbers
            ("1.0", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No floating point numbers
            ("1.e10", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No floating point numbers
            ("1.2e3", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No floating point numbers
            ("1e10", ParseError(InvalidSyntax)), // symbol
            // R7RS-RESTRICTED: No floating point numbers
            ("3.14", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No floating point numbers
            ("-1.5", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No floating point numbers
            (".5", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No rationals
            ("1/2", ParseError(InvalidSyntax)), // Parsed as symbol "1/2"
            // R7RS-RESTRICTED: No complex numbers
            ("1+2i", ParseError(InvalidSyntax)), // Parsed as symbol "1+2i"
            // R7RS-RESTRICTED: No complex numbers
            ("1.5+2.5i", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No binary number syntax
            ("#b101", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No binary number syntax & case-insensitive prefix
            ("#B101", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No binary number syntax
            (
                "#b11111111111111111111111111111111",
                ParseError(Unsupported),
            ),
            // R7RS-RESTRICTED: No octal number syntax
            ("#o10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No octal number syntax & case-insensitive prefix
            ("#O10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No octal number syntax
            ("#o77777777777", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No decimal prefix
            ("#d123", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No decimal prefix & case-insensitive prefix
            ("#D123", ParseError(InvalidSyntax)),
            // R7RS-RESTRICTED: No exactness prefix
            ("#e10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No exactness prefix & case-insensitive prefix
            ("#E10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No exactness prefix
            ("#e3.14", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No inexactness prefix
            ("#i10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No inexactness prefix & case-insensitive prefix
            ("#I10", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No inexactness prefix
            ("#i3.14", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No improper lists (dotted pairs)
            (".", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No improper lists (dotted pairs)
            ("(1 . 2)", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No character literals
            (r"#\a", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No character literals
            (r"#\space", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No vector syntax
            ("#(1 2 3)", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No bytevector syntax
            ("#u8(0 255)", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No quasiquote syntax
            ("`foo", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No unquote syntax
            (",foo", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No quasiquote syntax
            ("`(1 ,2)", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No datum labels for shared structures
            ("#1=(a #1#)", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No pipe-delimited symbols
            ("|symbol with spaces|", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No directives
            ("#!fold-case", ParseError(Unsupported)),
            // R7RS-RESTRICTED: No S-expression comments
            ("#;(1 2 3)", ParseError(Unsupported)),
            // ===== Standalone symbols that look like numbers =====
            ("+", success(sym("+"))),
            ("-", success(sym("-"))),
            ("+5", success(5)),
            ("-10.", ParseError(InvalidSyntax)),
            ("1a", ParseError(InvalidSyntax)),
        ];

        run_parse_tests(
            test_cases,
            ParseConfig {
                script_syntax: false, // Run most tests with script syntax off
            },
            "Default",
        );
    }

    #[test]
    fn test_script_syntax_features() {
        let script_tests = vec![
            // ===== Comments =====
            // Single-line comments
            ("42 ; this is a comment", success(42)),
            (
                "(+ 1 2) ; and another",
                precompiled_op("+", vec![val(1), val(2)]),
            ),
            // Block comments
            ("1 #| comment |# 2", ParseError(TrailingContent)), // Parses 1, rest is trailing
            ("#| comment |# 42", success(42)),
            (
                "(#| comment |# + 1 2)",
                precompiled_op("+", vec![val(1), val(2)]),
            ),
            (
                "(+ #| comment |# 1 2)",
                precompiled_op("+", vec![val(1), val(2)]),
            ),
            (
                "(+ 1 #| comment |# 2)",
                precompiled_op("+", vec![val(1), val(2)]),
            ),
            (
                "(+ 1 2 #| comment |#)",
                precompiled_op("+", vec![val(1), val(2)]),
            ),
            // Nested block comments
            ("#| outer #| inner |# outer |# 123", success(123)),
            // Unclosed block comments
            ("#| unclosed", ParseError(InvalidSyntax)),
            ("#| outer #| inner |# unclosed", ParseError(InvalidSyntax)),
            ("#| unclosed comment", ParseError(InvalidSyntax)),
            ("#| nested #| unclosed", ParseError(InvalidSyntax)),
            ("42 #| unclosed", ParseError(TrailingContent)), // Parses 42, then trailing unclosed comment
            // ===== Line Continuations in Strings =====
            ("\"line1\\\nline2\"", success("line1line2")),
            ("\"before\\\n   after\"", success("beforeafter")),
            ("\"before\\   \nafter\"", success("beforeafter")),
            ("\"before\\ \t \nafter\"", success("beforeafter")),
            ("\"before\\   \n   after\"", success("beforeafter")),
            ("\"a\\\nb\\\nc\"", success("abc")),
            ("\"before\\\n\\x0;after\"", success("before\0after")),
            // ===== Line Continuations Between Tokens =====
            ("(\\ \n + 1 2)", precompiled_op("+", vec![val(1), val(2)])),
            ("(+ 1 \\\n 2)", precompiled_op("+", vec![val(1), val(2)])),
            ("foo\\\nbar", ParseError(InvalidSyntax)), // Line continuation in middle of atom doesn't work
        ];

        run_parse_tests(
            script_tests,
            ParseConfig {
                script_syntax: true,
            },
            "ScriptSyntax",
        );

        let no_script_tests = vec![
            // ===== Comments (should be unsupported) =====
            ("; comment", ParseError(Unsupported)), // Comments require script_syntax
            ("42 ; comment", ParseError(Unsupported)), // Comments require script_syntax
            // R7RS-RESTRICTED: Block comments require script_syntax mode
            ("#| comment |# 42", ParseError(Unsupported)),
            // ===== Line Continuations (should be unsupported) =====
            // R7RS-RESTRICTED: Line continuations in strings require script_syntax mode
            ("\"line1\\\nline2\"", ParseError(Unsupported)),
            // R7RS-RESTRICTED: Line continuations between tokens require script_syntax mode
            ("(+ 1 \\\n 2)", ParseError(InvalidSyntax)), // Backslash in atom without script_syntax
        ];

        run_parse_tests(
            no_script_tests,
            ParseConfig {
                script_syntax: false,
            },
            "NoScriptSyntax",
        );
    }
}
