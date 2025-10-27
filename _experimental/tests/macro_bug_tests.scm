;; Test suite for Scheme macro expander bugs
;; Each section defines a macro that targets a specific bug and then calls it.
;; To run this file, use: cargo run --bin runscript -- superast tests/macro_bug_tests.scm

(display "--- Macro Bug Test Suite ---")
(newline)

;; -----------------------------------------------------------------------------
;; Bug 1: Incorrect Ellipsis Matching for List Sub-patterns
;;
;;  - Bug: The pattern `((name expr) ...)` fails to match `((a 1) (b 2))` because
;;    the matcher tries to match the sub-pattern `(name expr)` against a single
;;    wrapped item like `((a 1))` instead of the sublist `(a 1)`.
;;  - Expected Behavior: The macro should correctly destructure the inner lists.
;; -----------------------------------------------------------------------------

(define-syntax test-ellipsis-list
  (syntax-rules ()
    ((_ ((name expr) ...))
     (list 'names (list name ...) 'exprs (list expr ...)))))

(display "Bug 1: Ellipsis List Matching")
(newline)
(display "  Input: (test-ellipsis-list ((a 1) (b 2)))")
(newline)
(display "  Expected: (names (a b) exprs (1 2))")
(newline)
(display "  Actual: ")
(display (test-ellipsis-list ((a 1) (b 2))))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 2: Zero-Match Ellipsis Binding (Revised)
;;
;;  - Bug: When a nested ellipsis matches zero times, variables are not bound
;;    correctly. The simple case passed, but a nested case will fail.
;;  - Expected Behavior: `(test-zero-ellipsis-nested ((1) (2)))` should result in
;;    the inner `b ...` matching zero times for each outer loop, producing `((1 ()) (2 ()))`.
;; -----------------------------------------------------------------------------

(define-syntax test-zero-ellipsis-nested
  (syntax-rules ()
    ((_ ((a b ...) ...))
     (list (list a (list b ...)) ...))))

(display "Bug 2: Zero-Match Ellipsis (Nested)")
(newline)
(display "  Input: (test-zero-ellipsis-nested ((1) (2)))")
(newline)
(display "  Expected: ((1 ()) (2 ()))")
(newline)
(display "  Actual: ")
(display (test-zero-ellipsis-nested ((1) (2))))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 3: Template Expansion in Quoted Forms (Pattern vs. Literal)
;;
;;  - Bug: The expander may fail to substitute a pattern variable inside a
;;    quoted form. Or, it may incorrectly substitute a symbol that is intended
;;    to be a literal but shares a name with a pattern variable.
;;  - Expected Behavior: `(test-quote-collision 123)` should expand to
;;    `(list 'test 123)`. The `'test` should remain a literal symbol, while the
;;    unquoted `test` should be replaced by its value.
;; -----------------------------------------------------------------------------

(define-syntax test-quote-collision
  (syntax-rules ()
    ((_ test) ;; `test` is a pattern variable
     (list 'test test)))) ;; template uses 'test as literal and test as variable

(display "Bug 3: Quoted Pattern Variable Collision")
(newline)
(display "  Input: (test-quote-collision 123)")
(newline)
(display "  Expected: (list 'test 123)")
(newline)
(display "  Actual: ")
(display (test-quote-collision 123))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 4: Missing Hygiene
;;
;;  - Bug: A variable introduced by a macro template can accidentally capture
;;    a variable from the calling context if they share the same name.
;;  - Expected Behavior: The `my-let` macro uses `lambda` internally. When we
;;    define a variable named `lambda` in the outer scope, the macro should not
;;    be affected. The macro's `lambda` should be hygienic and distinct.
;;    The code should evaluate to 10, not produce an error.
;; -----------------------------------------------------------------------------

(define-syntax my-let
  (syntax-rules ()
    ((_ ((var val)) body)
     ((lambda (var) body) val))))

(display "Bug 4: Missing Hygiene")
(newline)
(display "  Input: (let ((lambda 5)) (my-let ((x 10)) x))")
(newline)
(display "  Expected: 10")
(newline)
(display "  Actual: ")
(let ((lambda 5))
  (display (my-let ((x 10)) x)))
(newline)
(newline)

;; -----------------------------------------------------------------------------
;; Bug 5: AST Pollution with define-syntax
;;
;; - Bug: `define-syntax` forms are not removed from the AST after expansion.
;; - Expected Behavior: The evaluator should not see `define-syntax` at all.
;;   This test defines a macro and then immediately tries to call it. If the
;;   evaluator sees `define-syntax`, it will error out.
;; -----------------------------------------------------------------------------

(display "Bug 5: AST Pollution")
(newline)
(display "  Input: (begin (define-syntax id (syntax-rules () ((_ x) x))) (id 1))")
(newline)
(display "  Expected: 1")
(newline)
(display "  Actual: ")
(display (begin (define-syntax id (syntax-rules () ((_ x) x))) (id 1)))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 6: Incorrect Ellipsis Escape Handling
;;
;; - Bug: The expander has a non-standard implementation for matching a literal
;;   `...` by using the form `(... <pattern>)`.
;; - Expected R7RS Behavior: A literal `...` should only be matched if it's
;;   included in the literals list of `syntax-rules`.
;; -----------------------------------------------------------------------------

(define-syntax test-literal-ellipsis
  (syntax-rules (...) ; R7RS way to make '...' a literal
    ((_ ...) 'found-literal-ellipsis)))

(display "Bug 6: R7RS Literal Ellipsis")
(newline)
(display "  Input: (test-literal-ellipsis ...)")
(newline)
(display "  Expected: found-literal-ellipsis")
(newline)
(display "  Actual: ")
(display (test-literal-ellipsis ...))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 7: Underscore `_` as a Wildcard
;;
;; - Bug: The `_` symbol is treated as a normal bindable variable, not a
;;   wildcard that can appear multiple times without conflict.
;; - Expected Behavior: A pattern with multiple `_` should match successfully
;;   without causing a "variable bound to multiple values" error.
;; -----------------------------------------------------------------------------

(define-syntax test-wildcard
  (syntax-rules ()
    ((_ _ _) 'two-wildcards)))

(display "Bug 7: Underscore Wildcard")
(newline)
(display "  Input: (test-wildcard 1 2)")
(newline)
(display "  Expected: two-wildcards")
(newline)
(display "  Actual: ")
(display (test-wildcard 1 2))
(newline)
(newline)


;; -----------------------------------------------------------------------------
;; Bug 8: Nested Ellipsis Depth Handling
;;
;; - Bug: The expander may not correctly handle patterns with multiple levels
;;   of ellipsis, failing to preserve the nested list structure.
;; - Expected Behavior: The pattern `((a ...) ...)` should match `((1 2) (3 4))`
;;   and the template `(list (list a ...) ...)` should reconstruct the nested
;;   structure, resulting in `((1 2) (3 4))`.
;; -----------------------------------------------------------------------------

(define-syntax test-nested-ellipsis
  (syntax-rules ()
    ((_ ((a ...) ...))
     (list (list a ...) ...))))

(display "Bug 8: Nested Ellipsis Depth")
(newline)
(display "  Input: (test-nested-ellipsis ((1 2) (3 4)))")
(newline)
(display "  Expected: ((1 2) (3 4))")
(newline)
(display "  Actual: ")
(display (test-nested-ellipsis ((1 2) (3 4))))
(newline)
(newline)

(display "--- Test Suite Complete ---")
(newline)
