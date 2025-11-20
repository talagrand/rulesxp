# R7RS Conformance Analysis for macros.rs

## Executive Summary
This document catalogs all deviations from R7RS Section 4.3 (syntax-rules) in the current `macros.rs` implementation, excluding hygiene (which is explicitly out of scope). Analysis performed October 28, 2025.

---

## CRITICAL BUGS (Preventing Core Functionality)

### 1. **Pattern Variable Collection Bug in Nested Ellipsis**
**Location:** `match_list_pattern_new()` lines 360-410  
**Severity:** CRITICAL  
**Status:** Active bug causing 29 test failures

**Problem:**
When matching nested ellipsis patterns like `((var ...) body) ...`, the variable collection logic incorrectly binds pattern variables to multiple unrelated values. For example, in pattern `(var val)`, the variable `var` gets bound to BOTH:
- The intended parameter list: `List([Symbol("proc"), Symbol("lst")])`  
- Unrelated body expressions: `List([Symbol("if"), ...])`

**Root Cause:**
The `collect_pattern_vars()` function collects ALL variables from the sub-pattern, but doesn't distinguish between variables at different structural positions. When matching `(var val)` against nested structures, it incorrectly associates values across different pattern positions.

**R7RS Violation:**
Section 4.3.2 requires pattern variables to match their specific structural position. Current implementation violates variable scoping within patterns.

**Impact:**
- Generates lambda expressions with non-symbol parameters
- All complex nested ellipsis macros fail
- Affects `let`, `case-lambda`, and any macro with `((vars ...) body) ...` patterns

**Fix Required:**
Rewrite variable collection to track structural context. Pattern variables must only bind to values at their exact structural position within the pattern.

---

## MAJOR CONFORMANCE ISSUES

### 2. **Improper Binding Value Structure for Nested Ellipsis**
**Location:** `match_list_pattern_new()` lines 395-402  
**Severity:** HIGH

**Problem:**
When a sub-ellipsis pattern creates `BindingValue::List`, the code flattens it to `Vec<Value>` and wraps each as `BindingValue::Single`. This loses the hierarchical structure needed for nested ellipsis expansion.

```rust
let sub_values: Vec<Value> = list.iter().map(|bv| match bv {
    BindingValue::Single(val) => val.clone(),
    BindingValue::List(_) => panic!("Unexpected nested list"),
}).collect();
var_collections.get_mut(var).unwrap().push(Value::List(sub_values));
```

**R7RS Violation:**
R7RS requires nested ellipsis patterns like `((a ...) ...)` to maintain separate repetition contexts for each ellipsis level. Flattening breaks this.

**Example:**
```scheme
;; Pattern: ((a ...) ...)
;; Input: ((1 2) (3 4 5))
;; Should bind: a → [[1, 2], [3, 4, 5]] (two-level structure)
;; Actually binds: a → [List([1, 2]), List([3, 4, 5])] (flattened with Value::List wrapper)
```

**Fix Required:**
Preserve `BindingValue::List(Vec<BindingValue>)` hierarchy without flattening.

---

### 3. **No Support for Improper Lists (Dotted Tail Patterns)**
**Location:** `parse_pattern()` lines 227-260  
**Severity:** HIGH

**Problem:**
R7RS allows patterns like `(a b . rest)` to match improper lists. Current implementation only handles proper lists via `Value::List`.

**R7RS Violation:**
Section 4.3.2: "An input form F matches a pattern P if and only if... P is of the form (P1 ... Pn . Pn+1) and F is a list or improper list..."

**Missing Features:**
- Cannot parse dotted tail in patterns: `(a b . rest)`
- Cannot match improper lists
- Cannot generate improper lists in templates

**Impact:**
- Cannot implement full R7RS `lambda` variadic syntax: `(lambda (a b . rest) ...)`
- Pattern matching limited to proper lists only

**Fix Required:**
1. Add `MacroPattern::DottedList(Vec<MacroPattern>, Box<MacroPattern>)` variant
2. Extend parser to recognize `.` in pattern lists
3. Update `match_pattern_new()` to handle improper list matching
4. Support improper list generation in templates

---

### 4. **No Vector Pattern Support**
**Location:** `parse_pattern()` and `match_pattern_new()`  
**Severity:** MEDIUM

**Problem:**
R7RS Section 4.3.2 specifies vector patterns: `#(P1 ... Pn)` should match vectors element-wise.

**R7RS Violation:**
"An input form F matches a pattern P if and only if... P is a vector of the form #(P1 ... Pn) and F is a vector..."

**Missing Features:**
- Cannot parse vector patterns: `#(a b c ...)`
- Cannot match against vector values
- Cannot generate vectors in templates

**Fix Required:**
1. Add `MacroPattern::Vector(Vec<MacroPattern>)` variant
2. Add `MacroTemplate::Vector(Vec<MacroTemplate>)` variant
3. Implement vector matching logic with ellipsis support
4. Handle vector construction in template expansion

---

### 5. **Underscore (`_`) Should Not Be Bindable in Templates**
**Location:** `parse_template()` lines 268-290 and `expand_template_with_quote()` lines 486-496  
**Severity:** MEDIUM

**Problem:**
The wildcard `_` is correctly handled in patterns (line 236), but in templates it's treated as a variable that gets bound.

**R7RS Requirement:**
R7RS Section 4.3.2: "It is an error for the same pattern variable to appear more than once in a pattern. The identifier `_` also matches any input..."

While not explicitly stated for templates, the convention is that `_` appearing in a template should be treated as a literal symbol, not looked up in bindings.

**Current Behavior:**
If `_` appears in a template and is somehow bound, it will be substituted. It should remain literal.

**Fix Required:**
In `expand_template_with_quote()`, special-case `_` to always return `Value::Symbol("_".to_string())` without checking bindings.

---

## MODERATE ISSUES

### 6. **Pattern Variable Uniqueness Not Enforced**
**Location:** `parse_pattern()` - no validation  
**Severity:** MEDIUM

**Problem:**
R7RS Section 4.3.2 states: "It is an error for the same pattern variable to appear more than once in a pattern."

Current implementation allows duplicate pattern variables without error:
```scheme
(define-syntax bad (syntax-rules () ((bad x x) 'duplicate)))  ;; Should error
```

**Fix Required:**
After parsing each pattern, collect all pattern variables and verify uniqueness. Throw error if duplicates found.

---

### 7. **No Proper Literal Matching Semantics**
**Location:** `match_pattern_new()` lines 313-315  
**Severity:** MEDIUM

**Problem:**
Current literal matching is simplistic:
```rust
(MacroPattern::Literal(lit), Value::Symbol(s)) if lit == s => true,
(MacroPattern::Literal(lit), other) if lit == &other.to_string() => true,
```

**R7RS Requirement:**
Literals should match by symbolic equality (identifier comparison), not string conversion. The second line converts ANY value to string for comparison, which is incorrect.

**Example Issue:**
```scheme
(define-syntax test (syntax-rules (42) ((test 42) 'matched)))
(test 42)  ;; Should match because 42 in pattern is literal in literals list
```

Current code might incorrectly match `42` as a literal without it being in the literals list.

**Fix Required:**
1. Literals in patterns should ONLY match symbols that are in the literals list
2. Non-symbol literals (numbers, strings) in patterns should be treated as literal data, not symbolic literals
3. Remove generic `.to_string()` comparison

---

### 8. **Ellipsis Variable Tracking Unused**
**Location:** `MatchContext` struct line 62-64  
**Severity:** LOW (code quality)

**Problem:**
```rust
struct MatchContext {
    bindings: PatternBindings,
    ellipsis_vars: HashMap<String, Vec<Value>>, // NEVER USED
}
```

The `ellipsis_vars` field is allocated but never read or written. This is dead code.

**Fix Required:**
Remove `ellipsis_vars` field entirely, or implement its intended purpose.

---

### 9. **No Ellipsis Escape Mechanism**
**Location:** `parse_pattern()` and `parse_template()`  
**Severity:** LOW

**Problem:**
R7RS doesn't provide a standard way to use `...` as a literal symbol in patterns/templates, but implementations typically need a way to escape it (e.g., `(... ...)` to mean literal `...`).

Current implementation has no escape mechanism. The symbol `...` always means ellipsis.

**R7RS Status:**
Not explicitly required, but good practice. Many R7RS implementations support `(... template)` as an escape.

**Fix Required:**
Optional - implement `(... template)` where the first `...` is literal and the second is the template.

---

## DESIGN ISSUES

### 10. **Single List Binding Workaround is a Hack**
**Location:** `expand_ellipsis_new()` lines 620-638 and 657-667  
**Severity:** MEDIUM (code quality)

**Problem:**
The code has special-case logic to handle `BindingValue::Single(Value::List(list))`:

```rust
} else if let Some(BindingValue::Single(Value::List(list))) = context.bindings.get(var) {
    // Handle the case where variable is bound to a single list value
    // In ellipsis context, this list should be expanded
    ellipsis_vars.insert(var.clone());
    // ...
```

This is a band-aid for the improper binding structure created by bug #2. If bindings were structured correctly, this wouldn't be needed.

**Fix Required:**
Fix the root cause (bug #2) and remove this workaround.

---

### 11. **Inconsistent Quote Handling**
**Location:** `expand_template_with_quote()` lines 489-512  
**Severity:** LOW

**Problem:**
The code checks for `quote` in TWO different ways:
1. As `MacroTemplate::Variable("quote")`
2. As `MacroTemplate::Literal(Value::Symbol("quote"))`

This suggests uncertainty about how `quote` gets parsed. Only one should be necessary.

**Fix Required:**
Determine canonical representation of `quote` in parsed templates and handle consistently.

---

### 12. **Error Messages Don't Include Source Context**
**Location:** Throughout, all `MacroError` constructions  
**Severity:** LOW (usability)

**Problem:**
All macro errors return generic messages without any source context:
```rust
return Err(MacroError("define-syntax requires exactly 2 arguments".to_string()));
```

**Best Practice:**
Include the actual syntax-rules form or pattern being processed in error messages for debugging.

**Fix Required:**
Enhance `MacroError` to include source expressions, or at least macro name in all errors.

---

### 13. **`MAX_EXPANSIONS` Constant May Be Too Low**
**Location:** Line 86  
**Severity:** LOW

**Problem:**
```rust
const MAX_EXPANSIONS: usize = 100;
```

For deeply nested macro expansions (e.g., recursive macros processing large lists), 100 iterations might be legitimately insufficient.

**Fix Required:**
Consider raising to 1000, or making it configurable.

---

## MISSING R7RS FEATURES (Low Priority)

### 14. **No Syntax-Case (Out of Scope)**
R7RS also defines `syntax-case` (Section 4.3.3), but this is typically considered an extension. Current focus is `syntax-rules` only.

### 15. **No Ellipsis Identifier Customization**
Some R7RS implementations allow specifying a different ellipsis identifier:
```scheme
(syntax-rules ::: (lit ...) rules)  ;; Use ::: instead of ...
```
Current implementation hard-codes `...`.

**Status:** Optional feature, not required for minimal R7RS compliance.

---

## SUMMARY TABLE

| # | Issue | Severity | R7RS Section | Status |
|---|-------|----------|--------------|--------|
| 1 | Pattern variable collection bug | CRITICAL | 4.3.2 | Active |
| 2 | Nested ellipsis binding structure | HIGH | 4.3.2 | Active |
| 3 | No improper list support | HIGH | 4.3.2 | Missing |
| 4 | No vector patterns | MEDIUM | 4.3.2 | Missing |
| 5 | Underscore in templates | MEDIUM | 4.3.2 | Missing |
| 6 | No duplicate pattern var check | MEDIUM | 4.3.2 | Missing |
| 7 | Improper literal matching | MEDIUM | 4.3.2 | Bug |
| 8 | Dead code (ellipsis_vars) | LOW | N/A | Cleanup |
| 9 | No ellipsis escape | LOW | Extension | Missing |
| 10 | Single list binding hack | MEDIUM | N/A | Design |
| 11 | Inconsistent quote handling | LOW | N/A | Design |
| 12 | Poor error messages | LOW | N/A | Quality |
| 13 | Low MAX_EXPANSIONS | LOW | N/A | Config |

---

## PRIORITY FIXES FOR BASIC R7RS CONFORMANCE

To achieve minimal R7RS `syntax-rules` conformance (without hygiene):

1. **Fix Bug #1** (Pattern variable collection) - Blocks everything
2. **Fix Bug #2** (Nested ellipsis binding structure) - Required for nested patterns
3. **Implement #3** (Improper list patterns) - Core R7RS feature
4. **Fix #6** (Duplicate pattern variable detection) - Error catching required by spec
5. **Fix #7** (Literal matching semantics) - Core pattern matching correctness

After these 5 fixes, the implementation would handle the most common R7RS `syntax-rules` cases correctly.

---

## TESTING RECOMMENDATIONS

For each fix, create targeted test cases:

1. **Bug #1**: Test `((var ...) body) ...` patterns with various nestings
2. **Bug #2**: Test `((a ...) ...)` with 3+ levels of nesting
3. **Feature #3**: Test `(a b . rest)` patterns with proper and improper lists
4. **Check #6**: Test patterns like `(x x)`, `(a b a)` expecting errors
5. **Bug #7**: Test literal matching with both symbols and non-symbols

---

*Analysis Date: October 28, 2025*  
*Analyzer: AI Assistant*  
*Scope: R7RS Section 4.3 (syntax-rules) excluding hygiene*
