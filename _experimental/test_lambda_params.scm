;; The exact failing case - ellipsis in lambda parameter list

(define-syntax make-lambda
  (syntax-rules ()
    ((_ (param ...) body)
     (lambda (param ...) body))))

;; Should expand to (lambda (x y) (+ x y))
(display ((make-lambda (x y) (+ x y)) 3 4))
(newline)
