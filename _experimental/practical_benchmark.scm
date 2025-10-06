;; Practical Complexity Benchmark - Testing Implementation Capabilities
;; Uses sophisticated constructs while staying within parser limits

;; =============================================================================
;; Y-COMBINATOR IMPLEMENTATION (LIMITED)
;; Fixed-point combinator without deep recursion
;; =============================================================================

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;; Non-recursive factorial generator
(define fact-gen
  (lambda (f)
    (lambda (n)
      (if (<= n 1)
          1
          (* n (f (- n 1)))))))

;; =============================================================================
;; CHURCH NUMERALS - PURE LAMBDA CALCULUS
;; =============================================================================

(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))
(define c4 (lambda (f) (lambda (x) (f (f (f (f x)))))))

;; Church arithmetic
(define c-succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

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
;; HIGHER-ORDER FUNCTION COMPOSITION
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
;; COMPLEX LIST PROCESSING (NON-RECURSIVE)
;; =============================================================================

(define process-three-items
  (lambda (lst)
    (if (null? lst)
        (list 0 1 0)
        (let ((item1 (car lst))
              (rest1 (cdr lst)))
          (if (null? rest1)
              (list item1 item1 1)
              (let ((item2 (car rest1))
                    (rest2 (cdr rest1)))
                (if (null? rest2)
                    (list (+ item1 item2) (* item1 item2) 2)
                    (let ((item3 (car rest2)))
                      (list (+ item1 item2 item3)
                            (* item1 item2 item3)
                            3)))))))))

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
      (multiplier 100)
      (base-factor 10))

  (let ((church-four (c-to-int c4))
        (church-sum (c-to-int ((c-add c2) c3)))
        (list-stats (process-three-items test-data))
        (decision-result (nested-decision 35))
        (arithmetic-result (complex-calc 13 17 19))
        (lambda-result nested-lambdas))

    ;; Final complex calculation
    (+ (* church-four multiplier)                     ; 400
       (* church-sum 150)                            ; 750
       
       ;; List processing results
       (let ((sum (car list-stats))
             (product (cadr list-stats))
             (count (caddr list-stats)))
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