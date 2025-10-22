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
;; ## PRIMITIVE FORMS (compiled natively, NOT macros):
;; - if, define, set!, lambda, quote, begin, letrec, letrec* (see src/processed_ast.rs)
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
;;
;; **R7RS DEVIATION:** Literal keywords (else, =>) in syntax-rules use structural matching
;; instead of free-identifier=? comparison. This means locally shadowing these keywords
;; (e.g., (let ((=> #f)) (cond (#t => 'ok)))) will still match the arrow syntax pattern
;; instead of treating => as a regular identifier. R7RS requires hygiene where shadowed
;; literals don't match patterns. **NEEDS-ENFORCEMENT:** Cannot be fixed without full
;; hygiene support in macro expander.
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result ...))
     (begin result ...))
    ((cond (test => proc))
     (let ((temp test))
       (if temp (proc temp))))
    ((cond (test => proc) clause ...)
     (let ((temp test))
       (if temp (proc temp) (cond clause ...))))
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
;; Also supports named let for recursion: (let name ((var val) ...) body ...)
;; **R7RS COMPLIANCE:** Empty bindings legal: (let () body) → ((lambda () body))
;; **R7RS RESTRICTED:** Named let limited to 1-4 bindings for implementation simplicity
(define-syntax let
  (syntax-rules ()
    ;; Named let with 3 bindings (must come before general ellipsis patterns)
    ((let name ((var1 val1) (var2 val2) (var3 val3)) body ...)
     ((letrec ((name (lambda (var1 var2 var3) body ...))) name) val1 val2 val3))
    
    ;; Named let with 2 bindings
    ((let name ((var1 val1) (var2 val2)) body ...)
     ((letrec ((name (lambda (var1 var2) body ...))) name) val1 val2))
    
    ;; Named let with 1 binding
    ((let name ((var val)) body ...)
     ((letrec ((name (lambda (var) body ...))) name) val))
    
    ;; Named let with 4 bindings
    ((let name ((var1 val1) (var2 val2) (var3 val3) (var4 val4)) body ...)
     ((letrec ((name (lambda (var1 var2 var3 var4) body ...))) name) val1 val2 val3 val4))
    
    ;; Regular let (non-recursive) - must come after named let patterns
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; let* - sequential binding, each binding sees previous ones
;; R7RS: (let* ((var val) ...) body ...)
;; **R7RS COMPLIANCE:** Empty bindings legal: (let* () body) → (begin body)
;; Expands to nested let forms for sequential evaluation
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (begin body ...))
    ((let* ((var val) binding ...) body ...)
     (let ((var val)) (let* (binding ...) body ...)))))

;; note letrec/letrec* are natively supported in the VM


;; ===== ITERATION FORMS =====
;; R7RS section 4.2.4 - derived expressions for iteration
;; **R7RS RESTRICTED:** do macro supports strict subset of R7RS syntax
;; **ENFORCED:** Unsupported patterns fail at macro expansion with clear error
;;
;; R7RS full `do` syntax: (do ((var init step) ...) (test expr ...) command ...) 
;; Our simplified version supports common cases with explicit patterns:
;; - 1-3 variables with optional step expressions
;; - Single test expression and result expression
;; - Optional command in body (do can have empty body)
;;
;; The full R7RS form requires nested ellipsis patterns like:
;; ((var init step ...) ...) where both step and the entire binding list can repeat
;; Our macro system cannot handle arbitrary nesting, so we enumerate common cases.
;;
;; **ENFORCEMENT:** Attempting to use unsupported patterns will fail at macro expansion:
;;   (do ((a 1 (+ a 1)) (b 2) (c 3) (d 4)) ...) → MacroError (too many vars)
;;
;; For complex iteration, use named let with explicit recursion.
(define-syntax do
  (syntax-rules ()
    ;; Two variables with steps, no body command
    ((do ((var1 init1 step1)
          (var2 init2 step2))
         (test expr))
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (loop step1 step2))))
    
    ;; Two variables with steps, with body command
    ((do ((var1 init1 step1)
          (var2 init2 step2))
         (test expr)
         command)
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (begin command (loop step1 step2)))))
    
    ;; Single variable with step, no body command
    ((do ((var init step))
         (test expr))
     (let loop ((var init))
       (if test
           expr
           (loop step))))
    
    ;; Single variable with step, with body command
    ((do ((var init step))
         (test expr)
         command)
     (let loop ((var init))
       (if test
           expr
           (begin command (loop step)))))
    
    ;; Single variable without step (same as init), no body
    ((do ((var init))
         (test expr))
     (let loop ((var init))
       (if test
           expr
           (loop init))))
    
    ;; Single variable without step, with body command
    ((do ((var init))
         (test expr)
         command)
     (let loop ((var init))
       (if test
           expr
           (begin command (loop init)))))
    
    ;; Two variables, first with step, second without, no body
    ((do ((var1 init1 step1)
          (var2 init2))
         (test expr))
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (loop step1 init2))))
    
    ;; Two variables, first with step, second without, with body
    ((do ((var1 init1 step1)
          (var2 init2))
         (test expr)
         command)
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (begin command (loop step1 init2)))))
    
    ;; Two variables, first without step, second with, no body
    ((do ((var1 init1)
          (var2 init2 step2))
         (test expr))
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (loop init1 step2))))
    
    ;; Two variables, first without step, second with, with body
    ((do ((var1 init1)
          (var2 init2 step2))
         (test expr)
         command)
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (begin command (loop init1 step2)))))
    
    ;; Two variables without steps, no body
    ((do ((var1 init1)
          (var2 init2))
         (test expr))
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (loop init1 init2))))
    
    ;; Two variables without steps, with body
    ((do ((var1 init1)
          (var2 init2))
         (test expr)
         command)
     (let loop ((var1 init1) (var2 init2))
       (if test
           expr
           (begin command (loop init1 init2)))))
    
    ;; Three variables with steps, no body
    ((do ((var1 init1 step1)
          (var2 init2 step2)
          (var3 init3 step3))
         (test expr))
     (let loop ((var1 init1) (var2 init2) (var3 init3))
       (if test
           expr
           (loop step1 step2 step3))))
    
    ;; Three variables with steps, with body
    ((do ((var1 init1 step1)
          (var2 init2 step2)
          (var3 init3 step3))
         (test expr)
         command)
     (let loop ((var1 init1) (var2 init2) (var3 init3))
       (if test
           expr
           (begin command (loop step1 step2 step3)))))))

;;; **R7RS RESTRICTED:** case-lambda - Only enumerates common arities (0-4 fixed args,
;;; optional rest args). Full R7RS case-lambda supports arbitrary arities via nested 
;;; ellipsis, which our macro system cannot handle. This covers ~95% of practical usage.
;; **R7RS case-lambda** - Hybrid implementation
;; Static dispatch for common small cases (0-3 args, 2-3 clauses)
;; Runtime dispatch for complex cases (4+ args or many clauses)
(define-syntax case-lambda
  (syntax-rules ()
    ;; Single clause - just wrap in lambda (no dispatch needed)
    ((case-lambda (formals body ...))
     (lambda formals body ...))
    
    ;; ===== STATIC DISPATCH - Optimized common patterns =====
    
    ;; Two clauses: zero args + one arg
    ((case-lambda (() body0 ...) ((x) body1 ...))
     (lambda args
       (if (null? args)
           (begin body0 ...)
           (if (and (pair? args) (null? (cdr args)))
               (let ((x (car args))) body1 ...)
               (error "case-lambda: wrong number of arguments")))))
    
    ;; Two clauses: one arg + two args
    ((case-lambda ((x) body1 ...) ((y z) body2 ...))
     (lambda args
       (if (and (pair? args) (null? (cdr args)))
           (let ((x (car args))) body1 ...)
           (if (and (pair? args) (pair? (cdr args)) (null? (cdr (cdr args))))
               (let ((y (car args)) (z (car (cdr args)))) body2 ...)
               (error "case-lambda: wrong number of arguments")))))
    
    ;; Three clauses: zero + one + two args
    ((case-lambda (() body0 ...) ((x) body1 ...) ((y z) body2 ...))
     (lambda args
       (if (null? args)
           (begin body0 ...)
           (if (and (pair? args) (null? (cdr args)))
               (let ((x (car args))) body1 ...)
               (if (and (pair? args) (pair? (cdr args)) (null? (cdr (cdr args))))
                   (let ((y (car args)) (z (car (cdr args)))) body2 ...)
                   (error "case-lambda: wrong number of arguments"))))))
    
    ;; ===== RUNTIME DISPATCH - General case =====
    ;; Handles: 4+ clauses, rest args, 3+ fixed args, or complex patterns
    ((case-lambda clause ...)
     ($case-lambda-dispatcher (clause ...)))))

;; Helper macro to generate the dispatcher
(define-syntax $case-lambda-dispatcher
  (syntax-rules ()
    (($case-lambda-dispatcher clauses)
     (lambda args
       (let ((len ($length* args)))
         ($case-lambda-match args len clauses))))))

;; Match arguments against clauses based on length
;; **R7RS RESTRICTED:** No dotted pair patterns, so rest args use symbol-only patterns
(define-syntax $case-lambda-match
  (syntax-rules ()
    ;; No more clauses - error
    (($case-lambda-match args len ())
     (error "case-lambda: no matching clause for" len "arguments"))
    
    ;; Empty parameter list (zero args) - last clause
    (($case-lambda-match args len ((() body ...)))
     (if (= len 0)
         (begin body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; One parameter - last clause
    (($case-lambda-match args len (((p1) body ...)))
     (if (= len 1)
         (let ((p1 (car args))) body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; Two parameters - last clause
    (($case-lambda-match args len (((p1 p2) body ...)))
     (if (= len 2)
         (let ((p1 (car args))
               (p2 (car (cdr args))))
           body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; Three parameters - last clause
    (($case-lambda-match args len (((p1 p2 p3) body ...)))
     (if (= len 3)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args)))))
           body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; Four parameters - last clause
    (($case-lambda-match args len (((p1 p2 p3 p4) body ...)))
     (if (= len 4)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args))))
               (p4 (car (cdr (cdr (cdr args))))))
           body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; Five parameters - last clause
    (($case-lambda-match args len (((p1 p2 p3 p4 p5) body ...)))
     (if (= len 5)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args))))
               (p4 (car (cdr (cdr (cdr args)))))
               (p5 (car (cdr (cdr (cdr (cdr args)))))))
           body ...)
         (error "case-lambda: no matching clause for" len "arguments")))
    
    ;; Rest args clause - single symbol, no parameter list (matches anything remaining)
    ;; This must come AFTER fixed-arg patterns - acts as catch-all for last clause
    ;; **R7RS RESTRICTED:** Cannot use dotted pair (a . rest), so single symbol binds to full args list
    (($case-lambda-match args len ((rest body ...)))
     (let ((rest args)) body ...))
    
    ;; ===== Patterns with remaining clauses =====
    
    ;; Empty parameter list with more clauses
    (($case-lambda-match args len ((() body ...) more-clause ...))
     (if (= len 0)
         (begin body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; One parameter with more clauses
    (($case-lambda-match args len (((p1) body ...) more-clause ...))
     (if (= len 1)
         (let ((p1 (car args))) body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; Two parameters with more clauses
    (($case-lambda-match args len (((p1 p2) body ...) more-clause ...))
     (if (= len 2)
         (let ((p1 (car args))
               (p2 (car (cdr args))))
           body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; Three parameters with more clauses
    (($case-lambda-match args len (((p1 p2 p3) body ...) more-clause ...))
     (if (= len 3)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args)))))
           body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; Four parameters with more clauses
    (($case-lambda-match args len (((p1 p2 p3 p4) body ...) more-clause ...))
     (if (= len 4)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args))))
               (p4 (car (cdr (cdr (cdr args))))))
           body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; Five parameters with more clauses
    (($case-lambda-match args len (((p1 p2 p3 p4 p5) body ...) more-clause ...))
     (if (= len 5)
         (let ((p1 (car args))
               (p2 (car (cdr args)))
               (p3 (car (cdr (cdr args))))
               (p4 (car (cdr (cdr (cdr args)))))
               (p5 (car (cdr (cdr (cdr (cdr args)))))))
           body ...)
         ($case-lambda-match args len (more-clause ...))))
    
    ;; Rest args with more clauses following - acts as catch-all
    ;; **R7RS RESTRICTED:** Cannot use dotted pair (a . rest), so single symbol binds to full args list
    (($case-lambda-match args len ((rest body ...) more-clause ...))
     (let ((rest args)) body ...))))

;; Note: $length* helper function is defined in functions.scm


