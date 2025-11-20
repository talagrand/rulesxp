; Test with quoted first element
(define-syntax test-optional-all
  (syntax-rules ()
    ((test-optional-all ((a b ...) ...))
     (list (list 'a b ...) ...))))

; Test case
(test-optional-all ((x 1 2) (y 3 4)))
