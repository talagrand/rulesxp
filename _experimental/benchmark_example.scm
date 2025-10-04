;; Enhanced benchmark with advanced Scheme constructs
;; Testing lambda functions, recursive procedures, let bindings, and higher-order functions

;; Lambda function for squaring numbers
(define square (lambda (x) (* x x)))

;; Recursive factorial function
(define factorial 
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

;; Higher-order function using map to process lists
(define squares-of-list
  (lambda (lst)
    (map square lst)))

;; Complex computation using all constructs
(let ((base-numbers (quote (1 2 3 4 5 6 7 8 9 10)))
      (multiplier 100))
  (let ((squared-numbers (squares-of-list base-numbers))
        (factorial-7 (factorial 7))
        (lambda-result ((lambda (x y z) (* x y z)) 42 17 23)))
    ;; Intensive arithmetic combining all results
    (+ 
       ;; Lambda and recursive function results
       factorial-7                                        ; 5040
       lambda-result                                      ; 16422
       multiplier                                         ; 100
       
       ;; Large multiplication chains
       (* 42 42 42 42 42)                                ; 130,691,232
       (* 37 37 37 37 37)                                ; 69,343,957
       (* 25 25 25 25 25)                                ; 9,765,625
       
       ;; Nested arithmetic operations with lambda calls
       (+ (* (square 10) 200 300) (* 150 (square 5) 350) (* 75 125 (square 7)))
       
       ;; Complex nested conditionals with function calls
       (if (< (square 6) 100)
           (+ (* 42 42) 
              (if (> (factorial 3) 5)
                  (* 42 42 42)
                  (* 42 42 42 42)))
           (* 42 42 42 42 42))
       
       ;; Deep conditional nesting with lambda
       (if (< 50 100)
           (if (< 60 100) 
               (if (< 70 100)
                   (if (< 80 100)
                       (if (< 90 100)
                           (* (square 10) 20 30 40)
                           (* 9 18 27 36 45))
                       (* 8 16 24 32 40))
                   (* 7 14 21 28 35))
               (* 6 12 18 24 30))
           (* 5 10 15 20 25))
       
       ;; Multiple arithmetic chains with let bindings
       (let ((a 1) (b 2) (c 3) (d 4) (e 5))
         (+ (+ (+ (+ a b) c) d) e))
       
       ;; Function composition and higher-order operations
       ((lambda (f x) (f (f x))) square 4)               ; Square of square of 4 = 256
       
       ;; Large constant calculations with function results
       (+ 123456 234567 345678 456789 (factorial 6))     ; Include factorial result
       (* 123 456 (square 7))                            ; Include lambda result
       (- 1000000 (factorial 5))                         ; Subtract factorial
       (/ 999999 3)                                      ; Division
       
       ;; Final complex nested expression with all constructs
       (* (+ (* (square 7) 8 9) (* 10 (factorial 3) 12) (* 13 14 15))
          (+ (* 16 17 18) (* 19 20 21) (* 22 23 24))
          (+ (* 25 26 27) (* 28 29 30) (* 31 32 33))))))