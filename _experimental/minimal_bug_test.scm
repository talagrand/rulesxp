;; Minimal reproduction of the bug
(define f (lambda (a) a))
(+ (f 1) 2)