;; Absolute minimal test - just one ellipsis expansion

(define-syntax simple
  (syntax-rules ()
    ((_ x ...)
     (quote (x ...)))))

(display (simple 1 2 3))
(newline)
