;; Minimal Sophisticated Benchmark 
;; Tests Y-combinator and Church numerals within system limits

;; Y-combinator implementation
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;; Church numerals
(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))

;; Church addition
(define c-add
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))

;; Convert Church numeral to integer
(define c-to-int
  (lambda (n)
    ((n (lambda (x) (+ x 1))) 0)))

;; Higher-order function composition
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

;; Complex nested lambda with arithmetic
(define complex-lambda
  ((lambda (a)
     ((lambda (b)
        (+ a b (* a 2) (* b 3)))
      42))
   17))

;; Main calculation
(let ((church-result (c-to-int ((c-add c2) c3)))  ; 5
      (compose-result ((compose (lambda (x) (* x 2)) (lambda (x) (+ x 10))) 15))  ; 50
      (nested-calc (let ((x 10) (y 20))
                     (+ (* x y) (- y x) (* x x) (* y y)))))  ; 610

  (+ church-result compose-result nested-calc complex-lambda))  ; Final result