;; Test without loading preludes

(define-syntax simple-test
  (syntax-rules ()
    ((_ (x ...))
     (lambda (y) (list x ...)))))

;; Should expand to (lambda (y) (list 1 2 3))
((simple-test (1 2 3)) 42)
