;; Test R7RS deviation validations

;; Should error: identifier-syntax
(define-syntax test-id (identifier-syntax 42))

;; Should error: make-variable-transformer  
(define-syntax test-var (make-variable-transformer (lambda (x) x)))

;; Should error: syntax-case
(define-syntax test-case (syntax-case () ()))

;; Should error: let-syntax
(let-syntax ((test (syntax-rules () [(test) 42])))
  (test))