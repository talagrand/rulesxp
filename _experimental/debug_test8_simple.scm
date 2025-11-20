(define-syntax test-debug
  (syntax-rules ()
    ((test-debug ((a b ...) ...))
     (list 'done))))

(test-debug ())
