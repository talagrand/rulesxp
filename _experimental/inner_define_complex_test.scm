;; Test inner defines with more complex cases
(define (test1) (define x 10) (define y (+ x 5)) y)
(define (test2) (define a 1) (define b 2) (define c (+ a b)) c)
(define (test3 n) (define double (* n 2)) (define triple (* n 3)) (+ double triple))

(test1)
(test2) 
(test3 4)