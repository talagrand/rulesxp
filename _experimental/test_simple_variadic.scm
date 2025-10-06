;; Simple variadic function test
;; Test basic variadic functionality without complex recursion

;; Define a simple variadic function that returns all arguments as a list
(define all-args (lambda args args))

;; Test with multiple arguments
(all-args 1 2 3)

;; Test with single argument
(all-args 42)

;; Test with no arguments (should return empty list)
(all-args)

;; Define a function that returns the first argument if any, otherwise 0
(define first-or-zero (lambda args
  (if (null? args)
      0
      (car args))))

;; Test the first-or-zero function
(first-or-zero 5 10 15)
(first-or-zero)