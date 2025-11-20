;; Tests for macro compiler error detection
;; These macros should be rejected at definition time with clear errors

;; Test: Variable used at inconsistent ellipsis depths
;; This should fail compilation
(define-syntax test-inconsistent-depth
  (syntax-rules ()
    ((_ (x ...) y)
     (list x ... y x))))  ;; ERROR: x used at depth 1 and depth 0

(display "If you see this, the inconsistent depth error wasn't caught!")
(newline)
