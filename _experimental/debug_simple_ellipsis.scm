;; Simple test to isolate the double-wrapping issue
;; This should show exactly where the problem occurs

(define-syntax simple-ellipsis-test
  (syntax-rules ()
    ((_ (arg ...) body)
     (lambda (arg ...) body))))

;; This should expand to: (lambda (a b) (+ a b))
;; NOT: (lambda ((a b)) (+ a b))
(simple-ellipsis-test (a b) (+ a b))