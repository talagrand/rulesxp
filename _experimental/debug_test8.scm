(define-syntax test-empty
  (syntax-rules ()
    ((test-empty ((a b ...) ...))
     (list (list 'a b ...) ...))))

(test-empty ())
