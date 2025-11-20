;; Test $do-iter in isolation

(define-syntax $do-iter
  (syntax-rules ()
    ;; Base case: The list of bindings is empty. Return the accumulated steps.
    ((_ () (steps ...))
     (steps ...))

    ;; Recursive case 1: The first binding includes a step expression.
    ;; Match (var init step), add 'step' to the accumulator, and recurse on the rest.
    ((_ ((var init step) rest ...) (steps ...))
     ($do-iter (rest ...) (steps ... step)))

    ;; Recursive case 2: The first binding does NOT include a step expression.
    ;; Match (var init), add 'var' to the accumulator, and recurse on the rest.
    ((_ ((var init) rest ...) (steps ...))
     ($do-iter (rest ...) (steps ... var)))))

;; Test it
($do-iter ((x 0 (+ x 1)) (y 10 (- y 1))) ())
