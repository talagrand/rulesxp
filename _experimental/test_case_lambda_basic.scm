;; Test case-lambda basic pattern - the one causing lambda parameter issues

(define-syntax case-lambda
  (syntax-rules ()
    ;; Single clause with formals list
    ((_ (formals body ...))
     (lambda formals body ...))
    
    ;; Multiple clauses - should dispatch based on argument count
    ((_ clause ...)
     (lambda args
       (case-lambda-dispatcher clause ...)))))

;; Test 1: Simple single-clause case-lambda
(case-lambda
  ((x y) (+ x y)))

;; Test 2: Multi-clause case-lambda
(case-lambda
  ((x) (* x x))
  ((x y) (+ x y)))
