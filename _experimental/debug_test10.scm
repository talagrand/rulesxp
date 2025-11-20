(define i 999)
(define j 888)

(define-syntax do-structure
  (syntax-rules ()
    ((do-structure ((var init step ...) ...) (test result ...))
     (list 'init-bindings (list (list 'var init) ...)
           'test test
           'results (list result ...)
           'steps (list (list step ...) ...)))))

(do-structure ((i 0 (+ i 1)) (j 10 (- j 1))) ((>= i 5) i j))
