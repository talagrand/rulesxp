;; Complex benchmark example for CPS vs non-CPS performance comparison
;; This example includes recursion, higher-order functions, conditionals, 
;; arithmetic, and function definitions to stress-test both compilation and evaluation

;; Recursive factorial with accumulator
(define factorial-acc
  (lambda (n acc)
    (if (= n 0)
        acc
        (factorial-acc (- n 1) (* n acc)))))

;; Factorial wrapper
(define factorial
  (lambda (n)
    (factorial-acc n 1)))

;; Higher-order function: apply function to list elements
(define map-list
  (lambda (f lst)
    (if (null? lst)
        '()
        (cons (f (car lst)) (map-list f (cdr lst))))))

;; Square function for mapping
(define square
  (lambda (x)
    (* x x)))

;; Fibonacci sequence (recursive, intentionally inefficient for benchmarking)
(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

;; Higher-order function composition
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; Complex nested calculation combining multiple features
(define complex-calc
  (lambda (n)
    (let ((fact-n (factorial n))
          (fib-n (fib n))
          (squared-nums (map-list square (cons n (cons (+ n 1) (cons (+ n 2) '()))))))
      (+ fact-n 
         fib-n 
         (car squared-nums)
         (car (cdr squared-nums))
         (car (cdr (cdr squared-nums)))))))

;; Main computation to benchmark
(complex-calc 8)