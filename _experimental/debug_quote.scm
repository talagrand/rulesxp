(define-syntax test-quote
  (syntax-rules ()
    ((test-quote x)
     (list 'test x))))

(test-quote 123)
