;; Test R7RS hygienic macros (simplified implementation)

;; Test basic macro hygiene - macro-introduced bindings should not capture user variables
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val)) body)
     (let ((var val)) body))))

;; This should work without variable capture
(define x 10)
(my-let ((y 5)) (+ x y))

;; Test variable capture prevention
(define-syntax bad-macro-attempt
  (syntax-rules ()
    ((bad-macro-attempt expr)
     (let ((temp expr)) (+ temp temp)))))

;; This should work hygienically - temp should not capture user's temp
(define temp 100)
(bad-macro-attempt 5)