;; Test case-lambda expansion in isolation
;; This is where the "lambda parameters must be symbols" error comes from

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (formals body ...))
     (lambda formals body ...))))

;; This should work - single clause
(display ((case-lambda ((x y) (+ x y))) 3 4))
(newline)
