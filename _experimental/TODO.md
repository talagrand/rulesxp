# TODO - Current Project State and Future Work

## Current Limitations

### R7RS RESTRICTED Features  
- **Numeric tower**: Only i64 integers (no floats, rationals, or arbitrary precision)
- **Variadic functions**: Only fully variadic `(lambda args body)` form supported
  - Dot notation `(lambda (a b . rest) body)` not implemented
- **Data types**: No vectors, bytevectors, characters, or improper lists
- **Modules**: Single global namespace, no R7RS library system
- **Old VM limitations**: Bytecode mode gracefully skips functions requiring unsupported features
  - Functions using `letrec`, `case-lambda`, or variadic lambdas are skipped during prelude load
  - Basic functions (`length`, `pair?`, etc.) work, but advanced functions (`map`, `filter`) are not available
  - Use SuperVM modes (superast/superstackast) for full functionality including advanced prelude functions

### R7RS DEVIATION Features
- **Continuations**: No `call/cc` or `dynamic-wind` support

## Active Tasks

### High Priority
- [x] Remove CPS support from compiler and evaluator
- [x] Clean up CPS-related files and references  
- [x] Update documentation to reflect CPS removal
- [x] Remove "noncps" backward compatibility alias from runscript
- [x] Fix old VM prelude loading issue - now gracefully skips unsupported functions (case-lambda, letrec)
- [x] Fix macro ellipsis depth validation - R7RS-compliant implementation (template depth >= pattern depth valid for replication, template depth < pattern depth invalid for flattening)
- [x] **PATTERN COMPILER IMPLEMENTED:** Two-phase macro compilation system
  - Separation: Compile patterns/templates at definition time, match/expand at use time
  - Pre-computed metadata: Variable sets, ellipsis depths, maximum nesting depth
  - **ISOLATED TO macro_compiler.rs**: Compilation logic separated from expansion (macros.rs)
  - Debug tool: debug_macro_only.rs dumps compiled metadata for validation
  - Validation: 19 comprehensive R7RS test cases all compile correctly
  - Early error detection: Inconsistent ellipsis depths caught at definition, not expansion
  - Validation results: Successfully compiled 19+ unique R7RS patterns with 100% accuracy
  - Error messages: Clear descriptions with macro name and rule number
  - R7RS coverage: All standard pattern/template forms validated
  - See COMPILER_VALIDATION.md for comprehensive test results
  - **R7RS ENFORCEMENT ADDED:**
    - ✓ Ellipsis position: Must be last element in pattern/template list
    - ✓ Ellipsis in literals: Rejected with error
    - ✓ Underscore in literals: Rejected with error (reserved for wildcard)
    - ✓ Standalone ellipsis: Cannot appear without preceding element
  - **DEPTH VALIDATION FIX:** Pattern variables vs. literals distinction
    - Was enforcing depth consistency for ALL identifiers (including `list`, `quote`, etc.)
    - Fixed analyze_template() to only validate pattern variables (passed as HashSet)
    - Literals (like `list`) can now appear at any depth in templates
    - Example: `(list (list a (list b ...)) ...)` - `list` at depths 0,1,2 now valid
  - **DATA STRUCTURE REFACTORING:** Improved naming and eliminated unnecessary tracking
    - Renamed `ellipsis_depth` → `variable_depths` in CompiledPattern (clearer: tracks pattern variables)
    - Renamed `ellipsis_depth` → `pattern_variable_depths` in CompiledTemplate (clearer: only pattern vars, not literals)
    - Eliminated literal tracking in template depths - literals can appear at any depth, don't need validation
    - Simplified analyze_template: only tracks pattern variables in depth HashMap, not all identifiers
    - Improved parameter naming throughout: `pattern_vars`, `var_depths`, `identifiers` vs generic `variables`
    - Template `variables` set still contains all identifiers (pattern vars + literals) for reference
    - Template `pattern_variable_depths` only contains pattern variables for depth validation
    - Result: Clearer intent, less memory usage, no "first instance wins" arbitrariness for literals
  - **STATUS:** Compiler phase complete and validated.
- [x] **PATTERN MATCHER IMPLEMENTED:** R7RS-compliant hierarchical binding system
  - **ISOLATED TO macro_matcher.rs**: Fresh implementation using compiled metadata
  - **R7RS COMPLIANCE:** Variables bind at correct ellipsis depth using pre-computed metadata
  - Hierarchical bindings: Direct (depth 0) and Repeated (depth N) with proper nesting
  - Depth validation: Variables checked against expected depth from compiler
  - All 5 comprehensive tests pass: simple vars, single ellipsis, nested ellipsis, empty ellipsis, multiple vars
  - Binding construction: Variables at depth N create Direct in temp context, ellipsis handler wraps into Repeated
  - **CRITICAL BUG FIX:** Variables now bind exactly once at correct depth (old bug: bound to lists instead of elements)
  - Test coverage: Depths 0, 1, 2, empty ellipsis, multiple variables in same ellipsis
  - Enhanced test suite: test_matcher_valid.scm with 30+ comprehensive R7RS patterns
  - **STATUS:** Matcher phase complete and validated.
- [x] **PATTERN EXPANDER IMPLEMENTED:** Template expansion using hierarchical bindings
  - **CREATED macro_expander.rs**: Fresh implementation using matcher bindings
  - Core functions: expand_template, expand_list_template, expand_ellipsis_template
  - Hierarchical expansion: Uses Binding structure from matcher (Direct/Repeated)
  - Unbound variables: Correctly treated as literal symbols (R7RS compliant)
  - Depth handling: Temp contexts for ellipsis expansion use depth 0 (Direct bindings)
  - All 5 comprehensive tests PASSING:
    - Test 1: Simple variable substitution (depth 0) ✓
    - Test 2: Single ellipsis expansion (depth 1) ✓
    - Test 3: Nested ellipsis expansion (depth 2) ✓
    - Test 4: Empty ellipsis (zero repetitions) ✓
    - Test 5: Multiple variables in ellipsis ✓
  - Test tool: test_expander_direct.rs validates expander in isolation
  - **LITERAL VS. PATTERN VARIABLE FIX:** Expander now distinguishes identifiers correctly
    - Was collecting ALL MacroTemplate::Variable nodes as ellipsis variables
    - Fixed expand_ellipsis_template() to filter using pattern_variables set (from compiled pattern)
    - API change: expand_template() accepts pattern_variables parameter
    - Literals like `list`, `quote` no longer cause "variable not found" errors in ellipsis
    - Example: `(list (list a (list b ...)) ...)` now expands correctly
  - **STATUS:** Expander phase complete and validated. All three phases (compiler, matcher, expander) working together.
- [x] **R7RS KEYWORD BINDING ENFORCEMENT:** ProcessedAST compilation checks
  - **ENFORCEMENT LOCATION:** ProcessedAST compilation (after macro expansion, before VM execution)
  - **RESERVED KEYWORDS:** 16 keywords protected from binding:
    - Core special forms: `if`, `lambda`, `define`, `quote`, `set!`, `begin`, `quasiquote`, `unquote`, `unquote-splicing`, `letrec`, `letrec*`, `define-syntax`, `syntax-rules`
    - Universal auxiliary keywords: `else`, `=>`, `_`, `...`
  - **BINDING CHECKS:** Added to all binding forms:
    - `define`: Variable and function name checks
    - `lambda`: Parameter list and variadic parameter checks
    - `set!`: Variable name checks
    - `letrec` and `letrec*`: Binding name checks
  - **TEST COVERAGE:** 33 test cases in test_keyword_binding_errors
    - All 13 core special forms tested
    - All 4 auxiliary keywords tested
    - Function definitions with keyword names
    - Lambda parameters (list and variadic forms)
    - set! with keywords
    - letrec/letrec* bindings with keywords
    - Positive tests: Similar names allowed (my-if, lambda-calculus)
  - **ERROR REPORTING:** Clear "keyword" error messages for all violations
  - **STATUS:** Complete and tested - all keyword binding attempts properly rejected

### Medium Priority
- [x] **FIXED do AND case-lambda MACROS:** Binding structure and rest-args support
  - **do macro fix:** Changed `$do-loop` accumulator from flat `(vars ...)` to proper binding pairs `((var init) ...)`
    - Was generating `(let loop (vars ...) ...)` instead of `(let loop ((var init) ...) ...)`
    - Fixed all three rules to build `(binding ...)` where each is `(var init)` pair
    - Now supports mixed step/no-step clauses (R7RS compliant): `(var init step)` or `(var init)`
    - Variables without step use their current value as step expression
    - test_improved_let_and_do_macros now passing (10 test cases including mixed steps)
  - **case-lambda fix:** Added rest-args pattern support for variadic clauses
    - Was only handling `((formals ...) body ...)` list patterns
    - Added separate rule for `(rest-param body ...)` symbol patterns (R7RS variadic)
    - Fixed pattern order: list pattern first (more specific), symbol pattern second (more general)
    - Rest-args expand to `(#t (apply (lambda rest-param body ...) rest-args))` for catch-all
    - Fixed `(length 'formals)` - quote needed for proper list length at expansion time
    - test_case_lambda now passing (19 test cases including 4+ args with rest patterns)
  - **TEST RESULTS:** comprehensive_evaluator_tests improved from 17/31 → 26/31 passing
    - Fixed do/let/case-lambda issues: +9 tests passing
    - 5 remaining failures are pre-existing macro tests (not regressions):
      * test_multi_ellipsis_macros
      * test_comprehensive_ellipsis_patterns  
      * test_macro_limitations_migrated
      * test_r7rs_compliance_fixes_2025_10_22
      * test_underscore_in_template_error
- [x] **FIXED MACRO LIMITATION:** Quote forms with pattern variables in ellipsis templates
  - Was treated as literals instead of template substitution: `'a` with pattern var `a` → literal `'a`
  - Fixed parse_template() to recursively parse quote contents: `'a` → `(quote <template-for-a>)`
  - Added find_max_binding_length() helper for complex ellipsis with nested templates like `(list 'a b ...)`
  - R7RS COMPLIANT: `(list 'a ...)` now correctly expands to `(list 'x 'y 'z)` not `(list)`
  - **CAVEAT:** Pattern variable names should not collide with desired literal symbols in quotes
    * Template `'test` where `test` is a pattern var will substitute the value
    * Use different names: pattern var `test-expr`, literal symbol `'test` in template
- [x] **FIXED MACRO BUG:** Empty list matching with nested ellipsis patterns
  - Pattern: `((a b ...) ...)` matching `()` incorrectly created mixed bindings
  - Was using sub-pattern's maximum depth for all variables, causing depth-0 vars to get List-wrapped
  - Fixed: All variables get empty bindings `[]` when ellipsis matches zero times (regardless of individual depth)
  - Added better error message: "used outside its ellipsis scope" instead of "bound to multiple values" for empty bindings
  - R7RS COMPLIANT: Empty matches now correctly produce empty results
- [x] **FIXED TEST SUITE:** Optional ellipsis comprehensive test suite (10 tests)
  - Tests 1-9: Pattern variable substitution in quotes, empty/non-empty ellipsis, nested structures
  - Test 10: Realistic do-loop structure with init/test/results/steps components
  - All tests passing with correct expectations
  - Test 5: Fixed variable values (i=999, j=777) to catch mismatches
- [ ] Improve error handling and reporting
- [ ] Add more comprehensive test coverage
- [ ] Optimize SuperVM performance
- [ ] Continue test binary consolidation - migrate remaining src/bin test files to comprehensive_evaluator_tests.rs

### Low Priority
- [ ] Consider adding more R7RS features within current constraints
- [ ] Improve REPL user experience
- [ ] Add more built-in functions

## Completed Cleanup Tasks
- [x] Removed duplicate test binaries: test_smart_macros.rs and test_updated_macros.rs
  - Both tested simple when macro (already in comprehensive tests line 1524-1530)
  - Both tested define-simple-macro (documented as unsupported at line 1531-1544)
  - Test cases fully covered in tests/comprehensive_evaluator_tests.rs
- [x] Removed debug_whitespace.rs - all whitespace/multi-line parsing already tested in comprehensive suite
- [x] Migrated and removed test_macro_limitations.rs
  - Preserved all test cases as documentation of what SHOULD work per R7RS
  - Disabled tests with clear limitation comments: optional ellipsis in nested patterns ((var init step ...) ...), dotted tail notation (arg ... . rest), call/cc support
  - Verified working features: nested ellipsis ((x ...) ...), multi-pattern with literals, context-dependent patterns
  - All working tests passing in test_macro_limitations_migrated()
  - **BUG DISCOVERED:** Pattern `((var init step ...) ...)` doesn't bind correctly - optional ellipsis within nested ellipsis fails
  - Macro parses but expands to literal `...` symbols instead of binding pattern variables
  - This blocks R7RS-compliant `do` macro with optional step expressions per variable
  - Current workaround: Our `do` macro requires all variables to have steps OR all to have no steps (uniform)