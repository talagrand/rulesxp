;; ===== SAMPLESCHEME MACRO PRELUDE =====
;; Standard Scheme macros implemented with syntax-rules

;; Logical operators
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2) (if test1 test2 #f))
    ((and test1 test2 test3) (if test1 (and test2 test3) #f))
    ((and test1 test2 test3 test4) (if test1 (and test2 test3 test4) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2) (if test1 test1 test2))
    ((or test1 test2 test3) (if test1 test1 (or test2 test3)))
    ((or test1 test2 test3 test4) (if test1 test1 (or test2 test3 test4)))))

;; Conditional expressions
(define-syntax when
  (syntax-rules ()
    ((when test expr) (if test expr))
    ((when test expr1 expr2) (if test (begin expr1 expr2)))
    ((when test expr1 expr2 expr3) (if test (begin expr1 expr2 expr3)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test expr) (if (not test) expr))
    ((unless test expr1 expr2) (if (not test) (begin expr1 expr2)))
    ((unless test expr1 expr2 expr3) (if (not test) (begin expr1 expr2 expr3)))))

;; Multi-way conditionals
(define-syntax cond
  (syntax-rules (else)
    ((cond (else result)) result)
    ((cond (else result1 result2)) (begin result1 result2))
    ((cond (test result)) (if test result))
    ((cond (test result1 result2)) (if test (begin result1 result2)))
    ((cond (test1 result1) (test2 result2)) 
     (if test1 result1 (cond (test2 result2))))
    ((cond (test1 result1) (test2 result2) (test3 result3))
     (if test1 result1 (cond (test2 result2) (test3 result3))))))