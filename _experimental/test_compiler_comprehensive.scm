;; Comprehensive R7RS pattern/template compiler validation
;; These should all compile successfully with correct metadata

;; Test 1: Simple pattern/template
(define-syntax test-simple
  (syntax-rules ()
    ((_ x)
     x)))

;; Test 2: Multiple variables
(define-syntax test-multi
  (syntax-rules ()
    ((_ a b c)
     (list a b c))))

;; Test 3: Single ellipsis
(define-syntax test-ellipsis
  (syntax-rules ()
    ((_ x ...)
     (list x ...))))

;; Test 4: Nested ellipsis (depth 2)
(define-syntax test-nested
  (syntax-rules ()
    ((_ (x ...) ...)
     (quote ((x ...) ...)))))

;; Test 5: Mixed depths
(define-syntax test-mixed
  (syntax-rules ()
    ((_ a (b ...) c)
     (list a (quote (b ...)) c))))

;; Test 6: Ellipsis in subpattern
(define-syntax test-subpattern
  (syntax-rules ()
    ((_ (a b ...) c)
     (cons a (list b ... c)))))

;; Test 7: Multiple ellipsis at same level
(define-syntax test-multi-ellipsis
  (syntax-rules ()
    ((_ (a ...) (b ...))
     (quote ((a ...) (b ...))))))

;; Test 8: Template with literals
(define-syntax test-literals
  (syntax-rules ()
    ((_ x)
     (if x 'yes 'no))))

;; Test 9: Multiple rules
(define-syntax test-multi-rule
  (syntax-rules ()
    ((_ x)
     x)
    ((_ x y)
     (list x y))
    ((_ x y z)
     (list x y z))))

;; Test 10: Pattern with list structure
(define-syntax test-list-pattern
  (syntax-rules ()
    ((_ (a . b))
     (cons a b))))

;; Test 11: Ellipsis with pair pattern
(define-syntax test-ellipsis-pair
  (syntax-rules ()
    ((_ (a b) ...)
     (quote ((a b) ...)))))

;; Test 12: Deep nesting (depth 3)
(define-syntax test-deep
  (syntax-rules ()
    ((_ ((x ...) ...) ...)
     (quote (((x ...) ...) ...)))))

;; Test 13: Variable at multiple depths (should error)
;; This tests that the compiler catches inconsistent depth usage
;; (define-syntax test-error-inconsistent
;;   (syntax-rules ()
;;     ((_ (x ...) y)
;;      (list x ... y x))))  ;; ERROR: x at depth 1 and 0

;; Test 14: Template variable not in pattern (literals)
(define-syntax test-template-literal
  (syntax-rules ()
    ((_ x y)
     (lambda (z) (list x y z)))))

;; Test 15: Complex real-world pattern (let-style)
(define-syntax test-let-style
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; Test 16: Wildcard pattern
(define-syntax test-wildcard
  (syntax-rules ()
    ((_ _ x)
     x)))

;; Test 17: Literals list
(define-syntax test-with-literals
  (syntax-rules (else =>)
    ((_ else x)
     x)
    ((_ => x)
     x)
    ((_ x)
     'other)))

;; Test 18: Empty list pattern
(define-syntax test-empty
  (syntax-rules ()
    ((_ ())
     'empty)
    ((_ x)
     'not-empty)))

;; Test 19: Recursive ellipsis in different positions
(define-syntax test-recursive-positions
  (syntax-rules ()
    ((_ x ... y z ...)
     (quote ((x ...) y (z ...))))))

;; Test 20: Ellipsis at end of sublist
(define-syntax test-ellipsis-end
  (syntax-rules ()
    ((_ (a b c ...))
     (quote (a b (c ...))))))

(display "All pattern/template compilations complete!")
(newline)
