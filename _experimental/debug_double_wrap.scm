;; Test case to debug the double-wrapping issue
;; Let's start with a simple case similar to what case-lambda might produce

(define-syntax test-wrap
  (syntax-rules ()
    ((_ (name arg ...) body)
     (lambda (name arg ...) body))))

;; This should produce: (lambda (proc lst) (+ proc lst))
;; But if double-wrapped would be: (lambda ((proc lst)) (+ proc lst))
(test-wrap (proc lst) (+ proc lst))