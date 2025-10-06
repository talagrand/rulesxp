;; Basic Complexity Benchmark - Using Only Basic Functions
;; Tests advanced constructs within implementation limitations

;; =============================================================================
;; Y-COMBINATOR IMPLEMENTATION
;; =============================================================================

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;; =============================================================================
;; CHURCH NUMERALS - PURE LAMBDA CALCULUS
;; =============================================================================

(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))
(define c4 (lambda (f) (lambda (x) (f (f (f (f x)))))))

;; Church arithmetic
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

;; =============================================================================
;; HIGHER-ORDER FUNCTIONS
;; =============================================================================

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define compose3
  (lambda (f g h)
    (lambda (x) (f (g (h x))))))

(define curry2
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (f x y)))))

;; =============================================================================
;; SIMPLE LIST PROCESSING (WITHOUT CADR/CADDR)
;; =============================================================================

(define process-simple-list
  (lambda (lst)
    (if (null? lst)
        (list 0 1 0)
        (let ((first (car lst))
              (rest (cdr lst)))
          (if (null? rest)
              (list first first 1)
              (let ((second (car rest))
                    (rest2 (cdr rest)))
                (if (null? rest2)
                    (list (+ first second) (* first second) 2)
                    (let ((third (car rest2)))
                      (list (+ first second third)
                            (* first second third)
                            3)))))))))

;; Manual list accessors to avoid cadr/caddr
(define second (lambda (lst) (car (cdr lst))))
(define third (lambda (lst) (car (cdr (cdr lst)))))

;; =============================================================================
;; NESTED CONDITIONALS
;; =============================================================================

(define nested-decision
  (lambda (x)
    (if (< x 50)
        (if (< x 25)
            (if (< x 10)
                (* x 10)
                (* x 5))
            (if (< x 40)
                (* x 3)
                (* x 2)))
        (if (< x 75)
            (if (< x 60)
                (+ x 20)
                (+ x 10))
            (if (< x 90)
                (- x 5)
                (/ x 2))))))

;; =============================================================================
;; COMPLEX ARITHMETIC
;; =============================================================================

(define complex-calc
  (lambda (a b c)
    (+ (* (+ a b) (- c a))
       (* (- b c) (+ a c))
       (* (+ b c) (- a b)))))

;; =============================================================================
;; NESTED LAMBDA EXPRESSIONS
;; =============================================================================

(define nested-lambdas
  ((lambda (f1)
     ((lambda (f2)
        ((lambda (f3)
           (+ (f1 10) (f2 20) (f3 30)))
         (lambda (x) (* x 3))))
      (lambda (x) (* x 2))))
   (lambda (x) x)))

;; =============================================================================
;; MAIN BENCHMARK
;; =============================================================================

(let ((test-data (quote (5 7 11)))
      (multiplier 100))

  (let ((church-four (c-to-int c4))
        (church-sum (c-to-int ((c-add c2) c3)))
        (list-stats (process-simple-list test-data))
        (decision-result (nested-decision 35))
        (arithmetic-result (complex-calc 13 17 19))
        (lambda-result nested-lambdas))

    ;; Final complex calculation
    (+ (* church-four multiplier)                     ; 400
       (* church-sum 150)                            ; 750
       
       ;; List processing results
       (let ((sum (car list-stats))
             (product (second list-stats))
             (count (third list-stats)))
         (+ (* sum 50)                               ; Process sum
            product                                  ; Add product
            (* count 1000)))                        ; Count bonus

       (* decision-result 25)                       ; Nested decision
       arithmetic-result                            ; Complex arithmetic
       lambda-result                                ; Nested lambdas

       ;; Function composition chain
       (((compose3 (lambda (x) (* x 2))
                   (lambda (x) (+ x 5))
                   (lambda (x) (* x x)))
         7)                                         ; (7^2 + 5) * 2 = 108
        *
        (((curry2 (lambda (a b) (+ (* a 3) b)))
          15) 25))                                  ; (15*3) + 25 = 70
       
       ;; Final nested computation
       (let ((x 20) (y 30) (z 40))
         (+ (* (+ x y) z)                           ; (20+30)*40 = 2000
            (* (- y x) z)                           ; (30-20)*40 = 400
            (* x (+ y z)))))))                      ; 20*(30+40) = 1400