;; Test R7RS hygienic macros (simplified implementation)
;; Use only built-in forms, no prelude dependencies

;; Test basic macro expansion
(define-syntax my-if
  (syntax-rules ()
    ((my-if test then else)
     (if test then else))))

;; Test simple expansion
(define x 10)
(my-if (= x 10) 42 0)

;; Test macro with identifier introduction
(define-syntax double
  (syntax-rules ()
    ((double expr)
     (+ expr expr))))

(double 21)