(define-syntax my-if (syntax-rules () [(my-if test then else) (if test then else)])) (my-if (= 5 5) 'true 'false)
