;; ===== CPS FUNCTION PRELUDE =====
;;
;; This file contains the function definitions for CPS (Continuation-Passing Style)
;; operations. These functions provide CPS-compatible versions of standard operations
;; and utility functions needed for CPS transformation.
;;
;; This file is automatically loaded as part of the function prelude along with
;; the standard Scheme functions.

;; Identity function - commonly used as final continuation
(define (identity x) x)

;; CPS-compatible arithmetic operators
;; These would replace the built-in operators in a full CPS system
(define ($cps-+ cont a b)
  (cont (+ a b)))

(define ($cps-- cont a b)
  (cont (- a b)))

(define ($cps-* cont a b)
  (cont (* a b)))

(define ($cps-/ cont a b)
  (cont (/ a b)))

;; CPS-compatible comparison operators
(define ($cps-= cont a b)
  (cont (= a b)))

(define ($cps-< cont a b)
  (cont (< a b)))

(define ($cps-> cont a b)
  (cont (> a b)))

;; CPS-compatible list operations
(define ($cps-cons cont car cdr)
  (cont (cons car cdr)))

(define ($cps-car cont lst)
  (cont (car lst)))

(define ($cps-cdr cont lst)
  (cont (cdr lst)))

(define ($cps-null? cont lst)
  (cont (null? lst)))