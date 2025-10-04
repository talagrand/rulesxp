;; Test file to verify CPS opcodes are generated

;; Simple nested arithmetic that should generate continuation opcodes
(+ 1 (+ 2 3))

;; Lambda that returns a continuation call  
((lambda (x) (+ x 10)) (+ 5 5))

;; More complex nested structure
(+ (+ 1 2) (+ 3 (+ 4 5)))