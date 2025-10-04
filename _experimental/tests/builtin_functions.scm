;; Test builtin functions and operations
;; Based on test_phase3_builtins.rs and other test files

;; Arithmetic operations
(+ 5 3 2)
(* 4 3)
(- 10 3)
(+ 5 3)

;; Comparison operations  
(= 42 42)
(= 42 24)
(< 3 5)
(< 5 3)
(> 5 3)
(> 3 5)

;; Nested arithmetic
(+ (* 2 3) 4)
(+ (* 2 3) (- 10 5))

;; Mixed operations
(if (> 5 3) (* 2 4) (+ 1 1))