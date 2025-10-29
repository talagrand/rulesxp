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

;; R7RS section 4.2.5 - derived expressions for case
;; This implementation ensures that <key> is evaluated only once.
(define-syntax case
  (syntax-rules (else)
    ((case key-expr clause ...)
     (let ((key key-expr))
       ($case-helper key clause ...)))))

(define-syntax $case-helper
  (syntax-rules (else)
    ;; Base case: No clauses match, result is unspecified. We return (void).
    (($case-helper key)
     (if #f #f))
    ;; 'else' clause must be last.
    (($case-helper key (else result ...))
     (begin result ...))
    ;; Standard clause with one or more datums.
    (($case-helper key ((datum ...) result ...) rest ...)
     (if (member key '(datum ...))
         (begin result ...)
         ($case-helper key rest ...)))))

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
;; R7RS-compliant 'do' macro using a recursive helper.
;; This version uses only proper lists in its patterns and templates.

(define-syntax $do-loop
  (syntax-rules ()
    ;; Recursive step for normalization with explicit step: (var init step).
    ;; Accumulates (var init) in bindings and step in steps.
    ((_ ((var init step) var-clause ...)
        (test result ...) (command ...)
        (binding ...) (step-exp ...))
     ($do-loop (var-clause ...)
              (test result ...) (command ...)
              (binding ... (var init)) (step-exp ... step)))

    ;; Recursive step for normalization without step: (var init).
    ;; Uses var as its own step expression.
    ((_ ((var init) var-clause ...)
        (test result ...) (command ...)
        (binding ...) (step-exp ...))
     ($do-loop (var-clause ...)
              (test result ...) (command ...)
              (binding ... (var init)) (step-exp ... var)))

    ;; Base case: All bindings processed, generate named let.
    ;; binding is now ((var init) ...) and step-exp is (step ...)
    ((_ () (test result ...) (command ...)
        (binding ...) (step-exp ...))
     (let loop (binding ...)
       (if test
           (begin result ...)
           (begin
             command ...
             (loop step-exp ...)))))
))

(define-syntax do
  (syntax-rules ()
    ;; Main entry point: passes bindings, test/result, commands,
    ;; and two empty accumulators for normalized bindings and step expressions.
    ((_ (var-clause ...) (test result ...) command ...)
     ($do-loop (var-clause ...) (test result ...) (command ...) () ()))))


;; **R7RS RESTRICTED:** Supports up to 5 args due to dependency on apply which has this limitation
(define-syntax case-lambda
  (syntax-rules ()
    ;; 1. Main entry point.
    ;; This part is unchanged. It calls the helper with the clauses
    ;; and an empty accumulator for the `cond` clauses.
    ((_ clause ...)
     ($case-lambda-helper (clause ...) ()))))

(define-syntax $case-lambda-helper
  (syntax-rules ()
    ;; 2a. Recursive step for fixed-arity pattern (list of formals)
    ;; This pattern matches when formals is a proper list with ellipsis.
    ;; MUST come before rest-args pattern to match properly.
    ((_ (((formal ...) body ...) rest-clauses ...) (cond-clause ...))
     ($case-lambda-helper
      (rest-clauses ...) ;; Recurse on the remaining clauses
      (cond-clause ...   ;; Keep the clauses we've already built
       ;; Add the new `cond` clause to the accumulator.
       ;; Note: '(formal ...) expands to a quoted list like '(x y) for length counting.
       ((= (length '(formal ...)) (length rest-args))
        (apply (lambda (formal ...) body ...) rest-args)))))

    ;; 2b. Recursive step for rest-args pattern (single identifier)
    ;; This pattern matches when formals is a single symbol (not a list).
    ;; In R7RS, a single identifier in case-lambda matches any number of args.
    ((_ ((rest-param body ...) rest-clauses ...) (cond-clause ...))
     ($case-lambda-helper
      (rest-clauses ...)
      (cond-clause ...
       ;; Rest args always match, so use #t as condition
       (#t (apply (lambda rest-param body ...) rest-args)))))

    ;; 3. Base case for recursion.
    ;; It triggers when the clause list is empty.
    ((_ () (cond-clause ...))
     (lambda rest-args
       (cond
         cond-clause ... ;; Splice in all the generated clauses
         (else
          (error "case-lambda: no matching clause for arguments" rest-args)))))))


