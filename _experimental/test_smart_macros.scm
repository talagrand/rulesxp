;; Test file for smart macro expansion

;; Define a simple macro
(define-syntax when
  (syntax-rules ()
    ((when test body)
     (if test body))))

;; Define a macro that generates another macro usage
(define-syntax define-simple-macro
  (syntax-rules ()
    ((define-simple-macro name replacement)
     (define-syntax name
       (syntax-rules ()
         ((name) replacement))))))

;; Test 1: Simple macro usage (should not need extra passes)
(when #t "This should work")

;; Test 2: Macro that generates macro usage (should need extra passes)
(define-simple-macro hello "Hello World!")
(hello)