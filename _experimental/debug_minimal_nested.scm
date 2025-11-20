;; Minimal test just for nested ellipsis without prelude interference

(define-syntax debug-nested-ellipsis
  (syntax-rules ()
    ((_ ((args ...) body) ...)
     ((lambda (args ...) body) ...))))

;; This should expand to: ((lambda (proc lst) (+ proc lst)))
(debug-nested-ellipsis ((proc lst) (+ proc lst)))