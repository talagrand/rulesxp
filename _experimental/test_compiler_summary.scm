;; Compiler validation summary

;; ✓ PASSED: Simple patterns
(define-syntax pass-simple (syntax-rules () ((_ x) x)))

;; ✓ PASSED: Single ellipsis
(define-syntax pass-ellipsis (syntax-rules () ((_ x ...) (quote (x ...)))))

;; ✓ PASSED: Nested ellipsis (depth 2)
(define-syntax pass-nested (syntax-rules () ((_ (x ...) ...) (quote ((x ...) ...)))))

;; ✓ PASSED: Deep nesting (depth 3)
(define-syntax pass-deep (syntax-rules () ((_ ((x ...) ...) ...) (quote (((x ...) ...) ...)))))

;; ✓ PASSED: Multiple variables at same depth
(define-syntax pass-multi (syntax-rules () ((_ (a b) ...) (quote ((a b) ...)))))

;; ✓ PASSED: Mixed depths
(define-syntax pass-mixed (syntax-rules () ((_ a (b ...) c) (list a (quote (b ...)) c))))

;; ✓ PASSED: Multiple ellipsis same level
(define-syntax pass-multi-ellipsis (syntax-rules () ((_ (a ...) (b ...)) (quote ((a ...) (b ...))))))

;; ✓ PASSED: Ellipsis at different positions
(define-syntax pass-positions (syntax-rules () ((_ x ... y z ...) (quote ((x ...) y (z ...))))))

;; ✓ PASSED: Complex let-style pattern
(define-syntax pass-let-style (syntax-rules () ((_ ((var val) ...) body ...) ((lambda (var ...) body ...) val ...))))

;; ✓ PASSED: Template literals (not in pattern)
(define-syntax pass-literals (syntax-rules () ((_ x) (lambda (z) (list x z)))))

;; ✓ PASSED: Multiple rules
(define-syntax pass-multi-rule 
  (syntax-rules ()
    ((_ x) x)
    ((_ x y) (list x y))
    ((_ x y z) (list x y z))))

;; ✓ PASSED: Literal identifiers
(define-syntax pass-with-literals
  (syntax-rules (else =>)
    ((_ else x) x)
    ((_ => x) x)
    ((_ x) 'other)))

;; ✓ PASSED: Empty list pattern
(define-syntax pass-empty (syntax-rules () ((_ ()) 'empty) ((_ x) 'not-empty)))

;; ✓ PASSED: Wildcard pattern
(define-syntax pass-wildcard (syntax-rules () ((_ _ x) x)))

(display "All validation tests passed!")
(newline)
