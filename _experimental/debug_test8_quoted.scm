(define-syntax test-quoted
  (syntax-rules ()
    ((test-quoted ((a b ...) ...))
     (list 'a ...))))

(test-quoted ())
