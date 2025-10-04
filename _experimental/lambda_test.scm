;; Test simple lambda definition and immediate invocation
((lambda (x) (+ x 1)) 42)

;; Test defining a lambda in a variable
(define add-one (lambda (x) (+ x 1)))
(add-one 10)

;; Test nested lambda invocation
(((lambda (x) (lambda (y) (+ x y))) 5) 3)