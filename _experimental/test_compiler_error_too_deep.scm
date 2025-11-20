;; More error detection tests

;; Error 2: Template variable deeper than pattern
(define-syntax test-error-too-deep
  (syntax-rules ()
    ((_ x)
     (list x ...))))  ;; ERROR: x at depth 0 in pattern, depth 1 in template

(display "Should not reach here!")
(newline)
