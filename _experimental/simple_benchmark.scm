;; Simple arithmetic and function benchmark  
;; Testing basic operations without recursion

(define square (lambda (x) (* x x)))

(define multiply-add (lambda (a b c) (+ (* a b) c)))

(define complex-calc 
  (lambda (x y z)
    (+ (* x x) (* y y) (* z z))))

(+ (square 10)
   (multiply-add 7 6 5)
   (* 42 42 42)
   (square 15)
   (complex-calc 3 4 5)
   (* (+ 10 20) (- 50 25))
   (if (< 10 20) 1000 500)
   (if (> (square 5) 20) 2000 1000)
   (multiply-add 100 200 300))