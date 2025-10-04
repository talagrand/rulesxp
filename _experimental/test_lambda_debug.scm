(debug-ast (lambda (x) (+ x 1)))
(debug-ast ((lambda (x) (+ x 1)) 42))
(exit)