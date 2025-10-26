;; Complex benchmark example
;; This example uses Y-combinator instead of direct recursion to avoid implementation limitations
;; Includes higher-order functions, conditionals, arithmetic, and function definitions

;; Y-combinator for recursion without self-reference (2-argument version)
(define Y2
  (lambda (f)
    ((lambda (x) (f (lambda (a b) ((x x) a b))))
     (lambda (x) (f (lambda (a b) ((x x) a b)))))))

;; Y-combinator for recursion without self-reference (1-argument version)
(define Y1
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;; Factorial with accumulator using Y-combinator (2 arguments)
(define factorial-acc
  (Y2 (lambda (self)
        (lambda (n acc)
          (if (= n 0)
              acc
              (self (- n 1) (* n acc)))))))

;; Factorial wrapper
(define factorial
  (lambda (n)
    (factorial-acc n 1)))

;; Map function using Y-combinator (2 arguments)
(define map-list
  (Y2 (lambda (self)
        (lambda (f lst)
          (if (null? lst)
              '()
              (cons (f (car lst)) (self f (cdr lst))))))))

;; Square function for mapping
(define square
  (lambda (x)
    (* x x)))

;; Fibonacci using Y-combinator (1 argument)
(define fib
  (Y1 (lambda (self)
        (lambda (n)
          (if (< n 2)
              n
              (+ (self (- n 1)) (self (- n 2))))))))

;; Higher-order function composition
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; Complex nested calculation combining multiple features - simplified without let
(define complex-calc
  (lambda (n)
    (+ (factorial n)
       (fib n)
       (* n n)
       (* (+ n 1) (+ n 1))
       (* (+ n 2) (+ n 2)))))

;; Main computation to benchmark
(complex-calc 5)
