;; Test case-lambda pattern matching issue

(define-syntax simple-case-lambda 
  (syntax-rules ()
    ((simple-case-lambda (formals body ...))
     (lambda formals body ...))))

(simple-case-lambda ((x y) (+ x y)))