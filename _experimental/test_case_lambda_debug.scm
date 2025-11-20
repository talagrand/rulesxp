;; Minimal case-lambda test
(define add (case-lambda (() 0) ((x) x) ((x y) (+ x y))))
(display (add))
(display (add 5))
(display (add 3 4))
