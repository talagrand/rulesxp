; Simpler test - quote inside ellipsis
(define-syntax test-quote-in-ellipsis
  (syntax-rules ()
    ((test-quote-in-ellipsis (a ...))
     (list 'a ...))))

(test-quote-in-ellipsis (x y z))
; Should expand to: (list 'x 'y 'z)
; Should evaluate to: (x y z)
