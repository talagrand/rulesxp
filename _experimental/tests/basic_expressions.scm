;; Basic expressions extracted from bin test files
;; Covers boolean literals, logical operations, conditionals, and simple forms

;; Boolean literals
#t
#f

;; Logical operations (from test_ellipsis_expansion.rs)
(and)
(and #t)
(and #t #f)
(and #t #t #t)
(or)
(or #f)
(or #f #t)
(or #f #f #t)

;; Conditional expressions (from test_interactive_cps.rs)
(if #t 10 20)
(if #f 10 20)
(if #t 42 0)

;; String literals
"hello"
"Hello World!"
"works"

;; Variable definition and reference (from test_cps_simple.rs)
(define x 42)
x

;; Simple expressions with literals
42
(+ 1 2)
(* 3 4)