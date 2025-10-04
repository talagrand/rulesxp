;; Intensive computational benchmark for CPS vs non-CPS performance comparison
;; This example focuses on heavy computation without recursion or Y-combinators
;; Features: deep function composition, complex arithmetic, higher-order functions, conditionals

;; Basic math utility functions
(define square
  (lambda (x)
    (* x x)))

(define cube
  (lambda (x)
    (* x (* x x))))

(define double
  (lambda (x)
    (* x 2)))

(define triple
  (lambda (x)
    (* x 3)))

(define increment
  (lambda (x)
    (+ x 1)))

(define decrement
  (lambda (x)
    (- x 1)))

;; Complex polynomial evaluation
(define poly3
  (lambda (x a b c d)
    (+ (* a (cube x))
       (* b (square x))
       (* c x)
       d)))

(define poly5
  (lambda (x a b c d e f)
    (+ (* a (cube (square x)))
       (* b (square (square x)))
       (* c (cube x))
       (* d (square x))
       (* e x)
       f)))

;; Function composition utilities
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define compose3
  (lambda (f g h)
    (lambda (x)
      (f (g (h x))))))

(define compose4
  (lambda (f g h i)
    (lambda (x)
      (f (g (h (i x)))))))

;; Deep computation chains
(define math-chain1
  (compose4 square cube double increment))

(define math-chain2
  (compose4 cube square triple decrement))

(define math-chain3
  (compose3 double square increment))

;; Conditional computation trees
(define complex-condition
  (lambda (x)
    (if (< x 5)
        (if (< x 3)
            (+ (* x x) (* x 2) 1)
            (+ (* x x x) (* x 4) 7))
        (if (< x 8)
            (+ (* x x x x) (* x 3) 12)
            (+ (* x x x x x) (* x 5) 20)))))

;; Nested arithmetic operations
(define nested-arithmetic
  (lambda (a b c)
    (+ (* (+ a b) (- c a))
       (* (- b c) (+ a c))
       (* (+ a c) (- b a))
       (square (+ a b c))
       (cube (- a b))
       (* (* a b) c))))

;; Higher-order function applications
(define apply-functions
  (lambda (x)
    (+ ((math-chain1) x)
       ((math-chain2) x)
       ((math-chain3) x)
       (complex-condition x))))

;; List creation and processing without recursion
(define process-list
  (lambda (a b c d e)
    (+ (car (cons a (cons b '())))
       (car (cdr (cons a (cons b (cons c '())))))
       (car (cdr (cdr (cons a (cons b (cons c (cons d '())))))))
       (car (cdr (cdr (cdr (cons a (cons b (cons c (cons d (cons e '())))))))))
       (nested-arithmetic a b c)
       (nested-arithmetic b c d)
       (nested-arithmetic c d e))))

;; Main intensive computation
(define intensive-calc
  (lambda (n)
    (+ (poly3 n 2 -3 4 -1)
       (poly5 n 1 -2 3 -4 5 -6)
       (apply-functions n)
       (process-list n (+ n 1) (+ n 2) (+ n 3) (+ n 4))
       (nested-arithmetic n (* n 2) (* n 3))
       (complex-condition (* n 2))
       ((compose3 square cube double) n)
       ((compose4 increment square decrement cube) n))))

;; Main computation to benchmark - intensive non-recursive calculation
(intensive-calc 8)