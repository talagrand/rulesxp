;; Test the optional ellipsis bug with do macro

(define-syntax do-step-or-var
  (syntax-rules ()
    ((do-step-or-var var step) step)
    ((do-step-or-var var) var)))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test result ...) command ...)
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (begin command ...
                  (loop (do-step-or-var var step ...) ...)))))
    ((do ((var init step ...) ...) (test result ...))
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (loop (do-step-or-var var step ...) ...))))))

;; Test 1: All variables with steps
(do ((x 0 (+ x 1))
     (y 10 (- y 1)))
    ((>= x 5) (list x y)))

;; Test 2: Mix of variables with and without steps (the bug)
(do ((x 0 (+ x 1))
     (y 10))
    ((>= x 5) (list x y)))

;; Test 3: No variables with steps
(do ((x 0)
     (y 10))
    ((>= x 5) (list x y)))
