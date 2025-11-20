;; Test if (x ... y) pattern is valid in R7RS

(define-syntax test-ellipsis-suffix
  (syntax-rules ()
    ((test-ellipsis-suffix (x ...) y)
     (list (list x ...) y))))

(test-ellipsis-suffix (1 2 3) final)
