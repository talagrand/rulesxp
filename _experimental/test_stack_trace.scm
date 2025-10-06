;; Test enhanced stack trace functionality
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; This should produce a stack trace when n is negative
(factorial -1)