;; ===== CPS MACRO PRELUDE =====
;;
;; This file contains the macro definitions for CPS (Continuation-Passing Style) 
;; transformation. These macros convert direct-style Scheme code into continuation-
;; passing style for advanced control flow features like call/cc.
;;
;; This file is automatically loaded by MacroExpander::load_prelude() along with
;; the standard R7RS derived expressions.
;;
;; Example transformation:
;;   (+ 1 2) => (+ 1 2 (lambda (result) result))
;;   (if test then else) => (cps-if test (lambda (test-val) 
;;                                          (if test-val then else)))
;;
;; This enables:
;; 1. Proper tail call optimization
;; 2. Foundation for call/cc implementation
;; 3. Explicit control flow representation

;; CPS transformation for basic expressions
;; IMPORTANT: Patterns are matched in order - most specific patterns first!
(define-syntax cps-transform
  (syntax-rules (lambda if define quote begin)
    
    ;; Quote - pass quoted value to continuation
    ((cps-transform (quote expr) k)
     (k (quote expr)))
    
    ;; Lambda - convert to CPS lambda (takes continuation as first argument)
    ((cps-transform (lambda (args ...) body) k)
     (k (lambda (cont args ...)
          (cps-transform body cont))))
    
    ;; If with else - CPS transform test, then branches
    ((cps-transform (if test then else) k)
     (cps-transform test (lambda (test-val)
                           (if test-val
                               (cps-transform then k)
                               (cps-transform else k)))))
    
    ;; If without else clause
    ((cps-transform (if test then) k)
     (cps-transform test (lambda (test-val)
                           (if test-val 
                               (cps-transform then k)
                               (k (if #f #f))))))  ; unspecified value
    
    ;; Begin - empty sequence
    ((cps-transform (begin) k)
     (k (if #f #f)))  ; unspecified value
    
    ;; Begin - single expression
    ((cps-transform (begin expr) k)
     (cps-transform expr k))
    
    ;; Begin - multiple expressions (sequence evaluation in CPS)
    ((cps-transform (begin expr rest ...) k)
     (cps-transform expr (lambda (_)
                           (cps-transform (begin rest ...) k))))
    
    ;; Define - variable definition with CPS value transformation
    ((cps-transform (define var val) k)
     (cps-transform val (lambda (cps-val)
                          (define var cps-val)
                          (k (if #f #f)))))  ; unspecified value
    
    ;; Define - function definition converted to CPS function
    ((cps-transform (define (name args ...) body) k)
     (begin
       (define name (lambda (cont args ...)
                      (cps-transform body cont)))
       (k (if #f #f))))  ; unspecified value
    
    ;; Application - CPS transform operator and operands, then apply
    ((cps-transform (op args ...) k)
     ($cps-app op (args ...) () k))
    
    ;; Catch-all for self-evaluating literals and variables - MUST BE LAST
    ((cps-transform val k) 
     (k val))))

;; Helper macro for CPS application
(define-syntax $cps-app
  (syntax-rules ()
    ;; Base case - all arguments transformed, now apply
    (($cps-app op () (cps-args ...) k)
     (op k cps-args ...))
    
    ;; Transform next argument
    (($cps-app op (arg rest-args ...) (cps-args ...) k)
     (cps-transform arg (lambda (cps-arg)
                          ($cps-app op (rest-args ...) (cps-args ... cps-arg) k))))))

;; Macro to convert a program to CPS with identity continuation
(define-syntax to-cps
  (syntax-rules ()
    ((to-cps expr)
     (cps-transform expr (lambda (result) result)))))