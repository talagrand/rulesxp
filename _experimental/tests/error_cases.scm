;; Test cases for errors and edge cases
;; This file tests error conditions and macro expansion failures

;; Valid expressions first
(+ 1 2)
"hello world"

;; Invalid expressions that should produce errors
(undefined-function 42)
(+ 1 "string")