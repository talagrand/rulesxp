;; Test to reproduce the exact case-lambda pattern that fails
;; This will help isolate the double-wrapping bug

(define-syntax debug-case-lambda
  (syntax-rules ()
    ((_ proc lst)
     (lambda (proc lst) (+ proc lst)))))

;; This should work correctly
(debug-case-lambda a b)

(define-syntax debug-nested-ellipsis  
  (syntax-rules ()
    ((_ ((args ...) body) ...)
     ((lambda (args ...) body) ...))))

;; This is where the double-wrapping likely occurs
(debug-nested-ellipsis ((proc lst) (+ proc lst)))