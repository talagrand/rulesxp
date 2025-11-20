;; Test case specifically for case-lambda pattern that causes double-wrapping

;; This simulates what case-lambda does - it should create lambda expressions
;; with proper parameter lists, not double-wrapped ones

(define-syntax test-case-lambda
  (syntax-rules ()
    ((_ ((args ...) body) ...)
     ((lambda (len)
        (if (= len 2)
          ((lambda ((args ...) body) ...) ...)
          (error "no match")))
      2))))

;; This should expand to something like:
;; ((lambda (len) (if (= len 2) ((lambda (proc lst) (+ proc lst))) (error "no match"))) 2)
;; NOT: ((lambda (len) (if (= len 2) ((lambda ((proc lst)) (+ proc lst))) (error "no match"))) 2)

(test-case-lambda ((proc lst) (+ proc lst)))