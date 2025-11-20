;; Test error detection in pattern compiler

;; Error 1: Variable at inconsistent depths
(define-syntax test-error-inconsistent
  (syntax-rules ()
    ((_ (x ...) y)
     (list x ... y x))))  ;; ERROR: x at depth 1 in ellipsis, depth 0 at end

(display "Should not reach here!")
(newline)
