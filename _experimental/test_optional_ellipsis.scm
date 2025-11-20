;; Test optional ellipsis within nested ellipsis
(define-syntax test-optional-ellipsis
  (syntax-rules ()
    ((test-optional-ellipsis ((var init step ...) ...))
     (quote ((var init step ...) ...)))))

;; Test cases
(display "Test 1 - all with steps: ")
(display (test-optional-ellipsis ((a 1 10) (b 2 20))))
(display "\n")

(display "Test 2 - mixed (some with steps, some without): ")
(display (test-optional-ellipsis ((a 1) (b 2 20))))
(display "\n")

(display "Test 3 - none with steps: ")
(display (test-optional-ellipsis ((a 1) (b 2))))
(display "\n")

(display "Test 4 - variable number of steps: ")
(display (test-optional-ellipsis ((a 1 10 100) (b 2))))
(display "\n")
