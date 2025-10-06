(define simple-recursive
  (lambda (n)
    (if (<= n 0)
        n
        (simple-recursive (- n 1)))))

(simple-recursive 5)