;; Hygiene Test

(define-syntax test-hygiene
  (syntax-rules ()
    ((_ val)
     (let ((a 10)) ; This 'a' should be hygienic and not conflict
       val))))

(display "New Hygiene test (should be 5): ")
(display (let ((a 5)) (test-hygiene a)))
(newline)
