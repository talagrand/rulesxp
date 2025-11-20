;; Simple matcher validation - no prelude macros
;; Tests R7RS pattern matching without complex macro dependencies

;; === DEPTH 0: Simple direct bindings ===
(define-syntax test-simple-var
  (syntax-rules ()
    ((_ x) x)))

;; === DEPTH 1: Single ellipsis ===
(define-syntax test-ellipsis-basic
  (syntax-rules ()
    ((_ x ...) (quote (x ...)))))

;; === DEPTH 2: Nested ellipsis ===
(define-syntax test-nested-simple
  (syntax-rules ()
    ((_ ((x ...) ...)) (quote ((x ...) ...)))))

;; === MIXED DEPTHS ===
(define-syntax test-mixed
  (syntax-rules ()
    ((_ prefix x ...) (quote (prefix x ...)))))

;; === LITERALS ===
(define-syntax test-literal
  (syntax-rules (=>)
    ((_ test => result) (quote (test result)))))

;; === WILDCARD ===
(define-syntax test-wildcard
  (syntax-rules ()
    ((_ _ x _) x)))

;; === MULTIPLE RULES ===
(define-syntax test-multi
  (syntax-rules ()
    ((_) (quote empty))
    ((_ x) (quote (single x)))
    ((_ x y) (quote (pair x y)))))

;; Test invocations
(test-simple-var 42)
(test-ellipsis-basic 1 2 3)
(test-nested-simple ((1 2) (3 4)))
(test-mixed prefix 1 2 3)
(test-literal foo => bar)
(test-wildcard ignore 99 also-ignore)
(test-multi)
(test-multi one)
(test-multi one two)
