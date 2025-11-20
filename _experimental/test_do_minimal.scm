;; Minimal reproduction - just the problematic do macro

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test result ...) command ...)
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (begin command ...
                  (loop (do-step-or-var var step ...) ...)))))))
