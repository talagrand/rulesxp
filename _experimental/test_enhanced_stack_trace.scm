;; Test enhanced stack trace functionality
;; This should produce a detailed stack trace showing argument values

(define (test-function x y z)
  (+ x (/ y z)))  ; This will error when z is 0

(define (wrapper a b)
  (test-function a b 0))  ; Pass 0 as third argument to cause division by zero

(wrapper 10 20)  ; This should show enhanced stack trace with argument values