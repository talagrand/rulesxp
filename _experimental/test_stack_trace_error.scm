;; Test enhanced stack trace functionality with an actual error
(define (test-func a b)
  (+ a b unknown-var))

(test-func 1 2)