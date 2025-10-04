;; Very intensive benchmark - testing computational limits
;; Deep nested arithmetic with complex conditional chains

(+ 
   ;; Large multiplication chains
   (* 42 42 42 42 42)                                    ; 130,691,232
   (* 37 37 37 37 37)                                    ; 69,343,957
   (* 25 25 25 25 25)                                    ; 9,765,625
   
   ;; Nested arithmetic operations
   (+ (* 100 200 300) (* 150 250 350) (* 75 125 175))   ; Nested multiplications
   
   ;; Complex nested conditionals with arithmetic
   (if (< 42 100)
       (+ (* 42 42) 
          (if (> 42 30)
              (* 42 42 42)
              (* 42 42 42 42)))
       (* 42 42 42 42 42))
   
   ;; Deep conditional nesting
   (if (< 50 100)
       (if (< 60 100) 
           (if (< 70 100)
               (if (< 80 100)
                   (if (< 90 100)
                       (* 10 20 30 40 50)
                       (* 9 18 27 36 45))
                   (* 8 16 24 32 40))
               (* 7 14 21 28 35))
           (* 6 12 18 24 30))
       (* 5 10 15 20 25))
   
   ;; Multiple arithmetic chains
   (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6)                      ; Deeply nested additions
   (+ (* 2 3) (* 4 5) (* 6 7) (* 8 9) (* 10 11))        ; Multiple multiplications
   (* (+ 10 20) (+ 30 40) (+ 50 60))                     ; Nested operations
   
   ;; Large constant calculations  
   (+ 123456 234567 345678 456789 567890)               ; Large numbers
   (* 123 456 789)                                       ; Medium multiplications
   (- 1000000 999999)                                    ; Large subtraction
   (/ 999999 3)                                          ; Division
   
   ;; Final complex nested expression
   (* (+ (* 7 8 9) (* 10 11 12) (* 13 14 15))
      (+ (* 16 17 18) (* 19 20 21) (* 22 23 24))
      (+ (* 25 26 27) (* 28 29 30) (* 31 32 33)))
)