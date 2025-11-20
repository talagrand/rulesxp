;; Test list-within-ellipsis pattern matching

(define-syntax extract-first
  (syntax-rules ()
    ((_ (a b) ...)
     (list a ...))))

(display (extract-first (1 2) (3 4)))
(newline)
