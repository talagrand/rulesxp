;; Test the do macro in isolation

(define-syntax do-loop
  (syntax-rules ()
    ((_ ((var init step) var-clause ...)
        (test result ...) (command ...)
        (vars ...) (steps ...))
     (do-loop (var-clause ...)
              (test result ...) (command ...)
              (vars ... var) (steps ... step)))

    ((_ ((var init) var-clause ...)
        (test result ...) (command ...)
        (vars ...) (steps ...))
     (do-loop (var-clause ...)
              (test result ...) (command ...)
              (vars ... var) (steps ... var)))

    ((_ () (test result ...) (command ...)
        (vars ...) (steps ...))
     (let loop (vars ...)
       (if test
           (begin result ...)
           (begin
             command ...
             (loop steps ...)))))))

(define-syntax do
  (syntax-rules ()
    ((_ (var-clause ...) (test result ...) command ...)
     (do-loop (var-clause ...) (test result ...) (command ...) () ()))))

;; Test
(do ((i 0 (+ i 1))) ((>= i 5) i))
