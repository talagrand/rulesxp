;; Test 1: Simple ellipsis - just copy values
(define-syntax test1
  (syntax-rules ()
    ((_ x ...)
     (list x ...))))

(display "Test 1: ")
(display (test1 1 2 3))
(newline)

;; Test 2: List-within-ellipsis - extract from pairs
(define-syntax test2
  (syntax-rules ()
    ((_ (a b) ...)
     (list a ...))))

(display "Test 2: ")
(display (test2 (1 2) (3 4) (5 6)))
(newline)

;; Test 3: Build lambda with ellipsis parameters (the failing case!)
(define-syntax test3
  (syntax-rules ()
    ((_ (var val) ...)
     (lambda (var ...) (list var ...)))))

(display "Test 3: ")
(display ((test3 (x 1) (y 2)) 10 20))
(newline)
