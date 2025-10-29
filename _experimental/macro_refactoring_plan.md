# Macro Expander Refactoring Plan

This document outlines the planned work to fix bugs and improve the R7RS conformance of the Scheme macro expander located in `src/macros.rs`. The tasks are based on the analysis of the test suite in `tests/macro_bug_tests.scm`.

The work is divided into two phases: first, we will refactor and simplify the existing code to make it more maintainable. Second, we will address the specific functional bugs.

---

## Phase 1: Code Cleanup and Refactoring

**Goal:** Simplify the codebase to make it easier to understand, maintain, and debug before tackling major functional bugs.

### 1.2. Consolidate Pattern Matching Functions

-   **Issue:** The existence of both `match_pattern` and `match_pattern_recursive` creates an unnecessary layer of indirection. `match_pattern` simply creates a `HashMap` and then calls `match_pattern_recursive`.
-   **Task:**
    -   Merge the logic into a single, primary function, likely `match_pattern_recursive`.
    -   The top-level caller will be responsible for creating the initial empty `PatternBindings` map.

### 1.3. Review and Clarify R7RS Deviation Comments

-   **Issue:** While many deviations are noted, some are missing or could be stated more clearly. A consistent and accurate set of comments is crucial for maintainability.
-   **Task:**
    -   Perform a thorough pass over `macros.rs`.
    -   Add, remove, or update comments related to R7RS conformance, restrictions, and deviations to ensure they accurately reflect the current state of the code.

---

## Phase 2: Bug Fixes

**Goal:** Address all functional bugs identified by the test suite.

### 2.1. Foundational: Fix Pattern Matching Logic (Bugs #1, #2, #7, #8)

-   **Issue:** The current pattern matcher incorrectly handles ellipsis, nested ellipsis, and wildcards, leading to "variable bound to multiple values" errors.
-   **Task:**
    -   Refactor the ellipsis matching logic in `match_list_pattern`. For each iteration of an ellipsis, use a temporary, isolated binding map.
    -   After each successful iteration, merge the temporary bindings into the main binding map, collecting values into lists for each pattern variable.
    -   Implement correct behavior for the `_` (underscore) wildcard, allowing it to match any value without creating a binding.
    -   Ensure the binding collection logic correctly preserves the structure for nested ellipsis patterns (e.g., `((a ...) ...)`).
    -   Fix the zero-match ellipsis case for nested patterns to ensure inner variables are bound to empty lists.

### 2.2. Architectural: Fix AST Pollution (Bug #5)

-   **Issue:** `define-syntax` forms are not consumed by the macro expander and are incorrectly passed to the compiler/evaluator.
-   **Task:**
    -   Modify `handle_define_syntax` to return `Ok(Value::Unspecified)` after successfully parsing and storing a macro, removing it from the subsequent AST.

### 2.3. Conformance: Fix Quoted Template Expansion (Bug #3)

-   **Issue:** The template expander incorrectly substitutes a pattern variable inside a `quote` form when the variable name clashes with an intended literal.
-   **Task:**
    -   This bug is closely related to hygiene. The fix will involve making the template instantiation logic aware of which symbols are pattern variables versus which are introduced literals that need to be hygienic.

### 2.4. Conformance: Implement Basic Hygiene (Bug #4)

-   **Issue:** The macro system is not hygienic, allowing variable capture.
-   **Task:**
    -   Implement a simple renaming (gensym) mechanism using `gensym_counter`.
    -   When parsing a rule, identify all symbols in the template that are *not* pattern variables. These are "introduced identifiers."
    -   During template instantiation (`instantiate_template`), replace these introduced identifiers with fresh, unique symbols.
    -   Track the mapping from the original introduced name to the generated name to ensure consistency within a single expansion.

### 2.5. Conformance: Fix Literal Ellipsis Handling (Bug #6)

-   **Issue:** The parser has a non-standard `(... <pattern>)` escape and fails to correctly parse the R7RS-compliant `(syntax-rules (...) ...)` form.
-   **Task:**
    -   Remove the non-standard ellipsis escape logic from `parse_pattern_with_literals`.
    -   Update `parse_syntax_rules` to correctly handle the case where `...` is present in the list of literals, treating it as a symbol to be matched literally.
