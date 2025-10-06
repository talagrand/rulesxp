;; Simple recursive factorial function test
(define factorial 
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

;; Test the function
(factorial 5)