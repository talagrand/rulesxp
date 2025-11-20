;; Test the actual do pattern that's failing

(define-syntax test-do-like
  (syntax-rules ()
    ((test-do-like ((var init step ...) ...) body)
     (let loop ((var init) ...)
       body
       (loop (helper var step ...) ...)))))

(test-do-like ((x 0 (+ x 1)) (y 10 (- y 1))) done)
