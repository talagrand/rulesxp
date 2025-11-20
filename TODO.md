# TODO List for rulesxp

## Completed Items
- [x] Full R7RS string escape sequence handling implemented
  - [x] Added support for `\a` (alarm/bell character)
  - [x] Added support for `\b` (backspace character)  
  - [x] Added support for `\x<hex>;` (hexadecimal Unicode escapes)
  - [x] Added support for full R7RS line continuations: `\<intraline whitespace>*<line ending><intraline whitespace>*`
  - [x] String escape sequences now 100% R7RS compliant
  - [x] Updated hexadecimal parsing to support both `#x` and `#X` (case-insensitive)
  - [x] Fixed nom 8.0 API compatibility issues throughout parser
  - [x] All tests passing with enhanced string parsing functionality
- [x] Enhanced error messages with context extraction
  - [x] Implemented custom `SchemeParseError` type capturing nom's `context()` labels
  - [x] All parsers return `SResult` using custom error type throughout chain
  - [x] Error messages now show parser context (e.g., "in number", "in symbol", "in string")
  - [x] Added "found" field showing problematic token at error site
  - [x] Simplified context extraction using `chars().take()` for cleaner code
  - [x] `ImplementationLimit` error kind for depth/overflow conditions
- [x] Dynamic error messages with closures (zero-cost abstraction)
  - [x] Implemented `SchemeParseError::error()` direct constructor returning `nom::Err`
  - [x] Added `ResultExt::context()` for attaching rich error messages
  - [x] First-wins strategy ensures closest error context is preserved
  - [x] Trait implementations map nom ErrorKind to ParseErrorKind automatically
  - [x] Custom messages for decimal overflow, hex overflow, invalid symbols
  - [x] Retained nom_kind field for fallback messages and trait system integration
  - [x] Removed nom's `context()` in favor of `.context()` extension method
  - [x] Removed `ContextError` trait implementation (no longer needed)
  - [x] Added explicit `.context()` calls throughout parser for better diagnostics
  - [x] All parser calls now have meaningful error messages attached
  - [x] Simplified type signatures: `SResult<'_, Value>` uses lifetime elision like `IResult`
  - [x] Function signatures match nom conventions: `fn parse(input: &str) -> SResult<'_, Value>`
- [x] Unicode support verified throughout parser
  - [x] Confirmed `char::is_whitespace()` provides full Unicode whitespace handling
  - [x] String parsing is fully Unicode-aware (no ASCII-only restrictions)
  - [x] Symbol character validation uses `is_alphanumeric()` (less generous than R7RS but Unicode-aware)
  - [x] Updated comments to reflect actual Unicode capabilities
- [x] Boolean syntax enhancements
  - [x] Added support for `#true` and `#false` (lowercase only, case-sensitive)
  - [x] Reordered parser alternates to match longer forms first (`#true` before `#t`)
- [x] Improper list detection via symbol validation
  - [x] Standalone `.` rejected as invalid symbol (reserved for dotted pair syntax)
  - [x] `+digit` and `-digit` patterns rejected (number syntax, not symbols)
  - [x] `.digit` patterns rejected (floating-point syntax, not symbols)
  - [x] Simplified list parser by removing redundant dotted-tail checking logic
  - [x] All improper list forms now properly rejected at parse time
- [x] Source code line continuations (R7RS §2.2)
  - [x] Implemented `\<ws>*<newline><ws>*` pattern between tokens
  - [x] Added support in `whitespace_or_comment()` function
  - [x] Tests for various line continuation patterns
- [x] Comprehensive R7RS parser compliance review completed
  - [x] Verified all unsupported features are intentionally restricted
  - [x] Confirmed implementation is complete for intended subset
- [x] Test framework improvements
  - [x] Refactored from string-based error checking (`SpecificError("text")`) to type-safe enum variants
  - [x] Added `AnyParseError` variant for generic parse errors
  - [x] Improved syntax with `ParseError(Kind)` pattern instead of `ParseErrorWithKind(ParseErrorKind)`
  - [x] Added wildcard import `use ParseErrorKind::*;` for concise variant names
  - [x] All scheme.rs tests updated and passing with new error checking
- [x] ParseError struct cleanup
  - [x] Removed deprecated `parser_label` field (always passed as None, never used)
  - [x] Reduced ParseError::new() from 5 parameters to 4
  - [x] Renamed `with_context_and_labels()` to `with_context_and_found()`
  - [x] Updated all call sites in lib.rs, scheme.rs, and jsonlogic.rs
  - [x] Kept `found` field (actively used, displays "Found: ..." in error messages)
- [x] REPL multi-line input support
  - [x] Detects incomplete input (unclosed parens, strings, comments) via ParseErrorKind::Incomplete
  - [x] Automatically prompts for continuation with `... >` prompt
  - [x] Accumulates input across multiple lines until expression is complete
  - [x] Ctrl+C clears accumulated input, second Ctrl+C exits
  - [x] Works with both Scheme and JSONLogic syntax
  - [x] Handles block comments, strings, and nested expressions
- [x] ParseConfig extended syntax flag
  - [x] Renamed `handle_comments` field to `extended_syntax` 
  - [x] Renamed `extended_syntax` to `script_syntax` with clear documentation
  - [x] Single flag now controls all R7RS extended features (comments + line continuations)
  - [x] Comments (`;`, `#|...|#`) require `script_syntax = true`
  - [x] Line continuations in strings (`\<ws>*<newline><ws>*`) require `script_syntax = true`
  - [x] Line continuations between tokens require `script_syntax = true`
  - [x] Default is `false` for strict subset mode
  - [x] Test suite updated to use new flag name
  - [x] REPL uses `script_syntax = true` for full R7RS feature support
  - [x] Validation centralized in `whitespace_or_comment()` with inline checks
  - [x] Block comment validation moved from `unsupported_forms` to `whitespace_or_comment()`
- [x] Test error documentation phase
  - [x] All `AnyParseError` instances replaced with specific `ParseError(Kind)` 
  - [x] Test expectations now document actual current behavior
  - [x] Test driver enhanced to show input expression in all error messages
  - [x] Documented behavior for all error cases:
    - String errors: "'" → InvalidSyntax, unterminated strings → InvalidSyntax
    - Delimiter errors: "(1 2 3" → InvalidSyntax, "1 2 3)" → TrailingContent
    - Empty input: "" → InvalidSyntax, whitespace → InvalidSyntax
    - Multiple expressions: "1 2" → TrailingContent
    - Improper lists: "(1 . 2)" → Unsupported
    - Floating point: ".5" → Unsupported
    - Comments without script_syntax: ";" → Unsupported, "#|" → Unsupported
  - [x] All 17 tests passing with fully documented actual behavior

## Current Limitations and Restrictions

### **R7RS-RESTRICTED:** String Processing
- Character literals (`#\space`, `#\newline`) not yet implemented
- String comparison functions may not handle Unicode normalization

### **R7RS-RESTRICTED:** Parser Features  
- Depth limit of 32 levels prevents extremely nested expressions
- Extended syntax features (comments, line continuations) are opt-in via `ParseConfig::extended_syntax`
- Block comments support basic nesting but may not handle all edge cases

### **R7RS-RESTRICTED:** Number System
- Limited to integer arithmetic only - no floating point support
- No complex numbers, rationals, or exact/inexact number distinctions
- Floating-point syntax (`.5`, `.123`, `3.14`, `1e5`, etc.) properly rejected at parse time

### **R7RS-RESTRICTED:** Data Types
- No improper lists (dotted pairs) - only proper lists supported
- No vector literals or bytevector support

## Testing Gaps
- Need comprehensive Unicode testing for hex escape sequences
- Edge cases for deeply nested string escapes
- Performance testing with large strings containing many escape sequences

## Testing Coverage
- Comprehensive test for all R7RS-RESTRICTED features validates proper error handling
- All unsupported syntax (character literals, floats, rationals, complex, vectors, bytevectors, dotted pairs) tested
- All incomplete input cases (unclosed delimiters, strings, comments) tested
- Number overflow and malformed syntax tested
- Trailing content detection tested

## Technical Debt
- `parse_hash_prefixed` function marked as unused - evaluate if needed or remove
- Consider consolidating escape sequence parsing logic for maintainability

## Future Enhancements
- Character literal support (`#\a`, `#\space`, etc.)
- String interpolation or formatting functions
- Better Unicode normalization handling
- Performance optimizations for string parsing