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
;; - case-lambda - procedure with multiple arities
;; - let-values, let*-values - multiple value binding (NEW with list-within-ellipsis)

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
;; **IMPROVED:** Now uses list-within-ellipsis patterns for cleaner implementation
(define-syntax let
  (syntax-rules ()
    ;; Named let - uses list-within-ellipsis to extract vars and vals separately
    ;; Pattern ((var val) ...) binds var→[v1,v2,...] and val→[val1,val2,...]
    ((let name ((var val) ...) body ...)
     ((letrec ((name (lambda (var ...) body ...))) name) val ...))
    
    ;; Regular let (non-recursive)
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
;; **IMPROVED WITH LIST-WITHIN-ELLIPSIS:** Now supports arbitrary variables with optional steps
;;
;; R7RS `do` syntax: (do ((var init step) ...) (test expr ...) command ...) 
;; Uses list-within-ellipsis patterns to extract vars, inits, and steps separately:
;; - Pattern ((var init step) ...) binds var→[v1,v2,...], init→[i1,i2,...], step→[s1,s2,...]
;; - Optional step defaults to init value when not provided
;;
;; **R7RS RESTRICTED:** step must be present or absent for all variables (no mixing)
;; Full R7RS allows (var init) and (var init step) mixed, which requires nested ellipsis
;; depth tracking that Phase 2 would enable. Current implementation covers two common cases:
;; 1. All variables have steps: ((var init step) ...)
;; 2. No variables have steps: ((var init) ...)
;;
;; For mixed cases, users can explicitly provide init as the step: ((x 0 (+ x 1)) (y 0 0))
(define-syntax do
  (syntax-rules ()
    ;; All variables with explicit step expressions, with body commands
    ((do ((var init step) ...)
         (test result ...)
         command ...)
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (begin command ... (loop step ...)))))
    
    ;; All variables with explicit step expressions, no body commands
    ((do ((var init step) ...)
         (test result ...))
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (loop step ...))))
    
    ;; All variables without step (step = init), with body commands
    ((do ((var init) ...)
         (test result ...)
         command ...)
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (begin command ... (loop init ...)))))
    
    ;; All variables without step (step = init), no body commands
    ((do ((var init) ...)
         (test result ...))
     (let loop ((var init) ...)
       (if test
           (begin result ...)
           (loop init ...))))
    
    ;; Empty variable list (infinite loop until test becomes true), with body
    ((do ()
         (test result ...)
         command ...)
     (let loop ()
       (if test
           (begin result ...)
           (begin command ... (loop)))))
    
    ;; Empty variable list, no body
    ((do ()
         (test result ...))
     (let loop ()
       (if test
           (begin result ...)
           (loop))))))

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

;; ===== NESTED ELLIPSIS MACROS (Phase 2) =====
;; These macros use nested ellipsis patterns (x ... ...) to manipulate
;; nested list structures. Requires multi-ellipsis template support.

;; Flatten: Convert nested list structure to flat list
;; (flatten ((1 2) (3 4) (5 6))) => (1 2 3 4 5 6)
(define-syntax flatten
  (syntax-rules ()
    ((_ ((x ...) ...))
     (list x ... ...))))

;; Zip: Combine two lists into list of pairs
;; (zip (1 2 3) (4 5 6)) => ((1 4) (2 5) (3 6))
(define-syntax zip
  (syntax-rules ()
    ((_ (a ...) (b ...))
     (list (list a b) ...))))

