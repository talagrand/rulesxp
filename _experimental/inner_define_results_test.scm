;; Test to see all results
(define (test1) (define x 10) (define y (+ x 5)) y)
(define (test2) (define a 1) (define b 2) (define c (+ a b)) c)

(list (test1) (test2))