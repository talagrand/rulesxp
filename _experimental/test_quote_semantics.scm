;; Test case: Does 'x in template mean literal symbol or quote the value?
(define-syntax test-quote-var
  (syntax-rules ()
    ((test-quote-var x)
     (list 'x x))))

(test-quote-var 123)
;; Expected: (list 'x 123) OR (list '123 123)?
