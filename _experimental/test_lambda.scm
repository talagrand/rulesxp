;; Test simple Y combinator (just structure, not full factorial)
(define Y
  (lambda (f)
    (lambda (x) (f x))))

;; Test it with a simple function
((Y (lambda (g) (lambda (n) (* n 2)))) 5)