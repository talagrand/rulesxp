(define i 999)
(define j 999)

(define-syntax do-bindings
  (syntax-rules ()
    ((do-bindings ((var init step ...) ...))
     (list (list 'var init (list step ...)) ...))))

(do-bindings ((i 0) (j 10)))
