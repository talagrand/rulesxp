# Macro Expander Refactoring Plan

This document outlines the necessary investments in `src/macros.rs` to achieve full R7RS compliance for derived expressions like `do`.

## 1. The Core Problem: Optional Patterns in Ellipsis

The current pattern-matching engine in `match_pattern_impl` cannot handle optional sub-patterns that appear within an ellipsis (`...`).

### 1.1. R7RS `do` Requirement

The `do` macro must support mixed variable bindings, where some variables have a `step` expression and others do not.

**Standard-Compliant Example:**
```scheme
(do ((x 0 (+ x 1))   ; Has a step
     (y 10))          ; Does not have a step
    ((> x 5) x)      ; Test and result
  (display x))       ; Body
```
Here, `x` is updated on each iteration, while `y` remains constant.

### 1.2. Our `syntax-rules` Pattern and Why It Fails

The pattern to support this is:
```scheme
(do ((var init step ...) ...) ; Problem is here
    (test result ...)
  command ...)
```

- The outer `...` matches the list of variable bindings: `((x 0 (+ x 1)) (y 10))`.
- The inner `step ...` is an optional pattern. It should match `(+ x 1)` for the first binding and *nothing* for the second.

Our implementation fails because it requires that all repetitions matched by an ellipsis have an identical structure. It cannot handle a scenario where `step ...` is present in one repetition but absent in another. This causes a "mis-alignment" of the captured pattern variables (`var`, `init`, `step`), leading to an expansion error because the lists of captured variables have different lengths.

## 2. Required Investment in `macros.rs`

To fix this, the pattern-matching logic in `match_pattern_impl` needs to be significantly enhanced.

**Proposed Change:**

When matching a pattern containing an ellipsis, if an optional sub-pattern (like `step ...`) is not found in a given repetition, the matcher must:

1.  **Recognize the Absence:** Detect that the optional sub-pattern is missing for that specific repetition.
2.  **Insert a Placeholder:** Instead of failing, it should insert a special, unique placeholder value into the list of captures for that pattern variable. This could be a specific `Value::Empty` or a similar sentinel.
3.  **Maintain Alignment:** This ensures that the capture lists for `var`, `init`, and `step` all have the same length, preserving the correct correspondence between them.

**Example Trace:**

For the binding `((x 0 (+ x 1)) (y 10))`, the captures should look like this:
- `var`: `(x y)`
- `init`: `(0 10)`
- `step`: `((+ x 1) <placeholder>)`

The template expander would then need to be taught how to handle this placeholder. For example, a helper macro could check for the placeholder and emit the correct code (either the step expression or the original variable).

## 3. Broader Impact

This is not just about `do`. Fixing this limitation would unlock the ability to implement other complex R7RS macros that rely on the same pattern, significantly improving our standards conformance. It is a foundational improvement to the macro system.
