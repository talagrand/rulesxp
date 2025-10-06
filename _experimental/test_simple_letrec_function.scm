;; Test letrec with simple function reference (not mutual)
(letrec ((f (lambda (x) (+ x 1))))
  (f 41))