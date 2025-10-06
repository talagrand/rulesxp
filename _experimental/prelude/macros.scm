;; ===== SAMPLESCHEME MACRO PRELUDE =====
;; 
;; This file contains all R7RS "derived expressions" - standard forms that are 
;; defined as macros rather than primitive compiler operations.
;;
;; CRITICAL: This file is automatically loaded by MacroExpander::load_prelude()
;; at startup. Without these macros, essential Scheme forms like (and), (or),
;; (let), (cond), etc. will not be available.
;;
;; All forms in this file are R7RS-compliant implementations using syntax-rules
;; with proper ellipsis (...) pattern matching and template expansion.
;;
;; ## R7RS Derived Expressions Implemented:
;; - and, or (4.2.1) - logical operators  
;; - when, unless (4.2.6) - simple conditionals
;; - cond, case (4.2.1, 4.2.5) - multi-way conditionals
;; - let, let* (4.2.2) - local binding forms
;; - do (4.2.4) - iteration form

;; ===== LOGICAL OPERATORS =====
;; R7RS section 4.2.1 - derived expressions for logical operations
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (if test1 test1 (or test2 ...)))))

;; ===== SIMPLE CONDITIONALS =====
;; R7RS section 4.2.6 - derived expressions for when/unless
(define-syntax when
  (syntax-rules ()
    ((when test expr ...)
     (if test (begin expr ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test expr ...)
     (if (not test) (begin expr ...)))))

;; ===== MULTI-WAY CONDITIONALS =====
;; R7RS sections 4.2.1, 4.2.5 - derived expressions for cond/case
(define-syntax cond
  (syntax-rules (else)
    ((cond (else result ...))
     (begin result ...))
    ((cond (test result ...))
     (if test (begin result ...)))
    ((cond (test result ...) clause ...)
     (if test (begin result ...) (cond clause ...)))))

;; Case expressions (simplified version)
(define-syntax case
  (syntax-rules (else)
    ((case key (else result ...))
     (begin result ...))
    ((case key ((values ...) result ...))
     (if (member key '(values ...)) (begin result ...)))
    ((case key ((values ...) result ...) clause ...)
     (if (member key '(values ...)) 
         (begin result ...) 
         (case key clause ...)))))

;; ===== LOCAL BINDING FORMS =====
;; R7RS section 4.2.2 - derived expressions for local variable binding

;; Basic let - transforms to lambda application
;; R7RS: (let ((var val) ...) body ...)
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; let* - sequential binding, each binding sees previous ones
;; R7RS: (let* ((var val) ...) body ...)
;; Expands to nested let forms for sequential evaluation
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (begin body ...))
    ((let* ((var val) binding ...) body ...)
     (let ((var val)) (let* (binding ...) body ...)))))

;; letrec - parallel binding for mutual recursion
;; R7RS: (letrec ((var val) ...) body ...)
;; This is a PRIMITIVE form handled by the VM, not a macro
;; Included here for documentation - the VM directly evaluates letrec

;; letrec* - sequential binding with forward reference support
;; R7RS: (letrec* ((var val) ...) body ...)
;; Unlike let*, later bindings can reference earlier ones (like letrec)
;; but evaluation is left-to-right sequential (like let*)
;;
;; **IMPLEMENTATION NOTE:** This expands to nested letrec forms
;; Each binding gets its own letrec scope so later inits can reference it
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* () body ...)
     (begin body ...))
    ((letrec* ((var val)) body ...)
     (letrec ((var val)) body ...))
    ((letrec* ((var1 val1) (var2 val2) binding ...) body ...)
     (letrec ((var1 val1))
       (letrec* ((var2 val2) binding ...) body ...)))))

;; ===== ITERATION FORMS =====
;; R7RS section 4.2.4 - derived expressions for iteration
;; **R7RS DEVIATION:** do macro simplified due to nested ellipsis pattern limitations
;; **NEEDS-ENFORCEMENT**
;;
;; R7RS full `do` syntax: (do ((var init step) ...) (test expr ...) command ...) 
;; Our simplified version only supports:
;; - Single variable binding: (do ((var init)) (test expr) command)
;; - Single test expression and result expression (not multiple)
;; - Single command in body (not multiple commands)
;;
;; The full R7RS form requires nested ellipsis patterns like:
;; ((var init step ...) ...) where both step and the entire binding list can repeat
;; Our macro system cannot handle this level of pattern complexity yet.
;;
;; This covers most practical use cases while staying within our macro system's
;; capabilities. For complex iteration, use named let with explicit recursion.
(define-syntax do
  (syntax-rules ()
    ((do ((var init))
         (test expr)
         command)
     (let loop ((var init))
       (if test
           expr
           (begin command (loop var)))))))