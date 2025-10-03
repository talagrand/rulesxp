;; Test ellipsis expansion
(and #t #f #t)
(or #f #t #f)
(when #t (display "hello") (display "world"))
(cond (#f "no") (#t "yes") (else "default"))