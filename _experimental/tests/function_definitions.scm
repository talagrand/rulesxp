;; Function definitions and lambda expressions extracted from bin test files
;; Covers function definitions, lambda expressions, and function applications

;; Simple function definition (from test_cps_simple.rs)
(define (identity x) x)

;; Lambda expressions (from test_enhanced_cps.rs)
(lambda (x) x)
(lambda (x y) (+ x y))

;; Function application
((lambda (x) x) 42)
((lambda (x y) (+ x y)) 3 4)

;; Function definition and usage
(define (square x) (* x x))
(square 5)

;; Named function with multiple expressions
(define test-expr (+ 1 2))
test-expr

;; More complex lambda with body
(lambda (x) (+ x 1))

;; Function returning a constant
(define (get-forty-two) 42)
(get-forty-two)