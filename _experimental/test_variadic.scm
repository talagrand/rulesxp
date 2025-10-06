;; Test fully variadic functions (lambda args body) 
;; All arguments go into a list

;; Define a simple variadic function that returns the first argument
(define first-arg (lambda args (car args)))

;; Test with multiple arguments
(first-arg 1 2 3 4)

;; Define a function that returns all arguments as a list
(define all-args (lambda args args))

;; Test with different numbers of arguments
(all-args)
(all-args 42)
(all-args 1 2 3)

;; Define a function that sums all arguments
(define sum-all (lambda args
  (if (null? args)
      0
      (+ (car args) (apply sum-all (cdr args))))))

;; Test the sum function
(sum-all 1 2 3 4)

;; Test edge cases
(sum-all)
(sum-all 42)