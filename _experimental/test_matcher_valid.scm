;; Comprehensive matcher test suite - valid R7RS patterns only
;; Tests all binding depths and edge cases

;; === DEPTH 0: Simple direct bindings ===
(define-syntax test-simple-var
  (syntax-rules ()
    ((_ x) x)))

(define-syntax test-simple-multi
  (syntax-rules ()
    ((_ x y z) (quote (x y z)))))

;; === DEPTH 1: Single ellipsis ===
(define-syntax test-ellipsis-basic
  (syntax-rules ()
    ((_ x ...) (quote (x ...)))))

(define-syntax test-ellipsis-pair
  (syntax-rules ()
    ((_ (name val) ...) (quote ((name val) ...)))))

;; === DEPTH 2: Nested ellipsis ===
(define-syntax test-nested-simple
  (syntax-rules ()
    ((_ ((x ...) ...)) (quote ((x ...) ...)))))

(define-syntax test-nested-pair
  (syntax-rules ()
    ((_ ((a b) ...) ...) (quote (((a b) ...) ...)))))

;; === MIXED DEPTHS: Regular + ellipsis ===
(define-syntax test-mixed-before
  (syntax-rules ()
    ((_ prefix x ...) (quote (prefix x ...)))))

;; Note: Cannot have ellipsis before suffix in R7RS - ellipsis must be last
;; Invalid: ((_ x ... suffix) ...) - ellipsis not at end
;; Invalid: ((_ prefix x ... suffix) ...) - ellipsis not at end

;; === COMPLEX PATTERNS ===
(define-syntax test-let-like
  (syntax-rules ()
    ((_ ((var val) ...) body) 
     (quote (((var val) ...) body)))))

;; === LITERALS ===
(define-syntax test-literal
  (syntax-rules (=>)
    ((_ test => result) (quote (test result)))))

(define-syntax test-multiple-literals
  (syntax-rules (lambda let if)
    ((_ lambda x) (quote (lambda-form x)))
    ((_ let x) (quote (let-form x)))
    ((_ if x) (quote (if-form x)))))

;; === WILDCARD ===
(define-syntax test-wildcard
  (syntax-rules ()
    ((_ _ x _) x)))

(define-syntax test-wildcard-ellipsis
  (syntax-rules ()
    ((_ _ ...) (quote matched-wildcards))))

;; === EDGE CASES ===
(define-syntax test-empty-ellipsis
  (syntax-rules ()
    ((_ x ...) (quote (x ...)))))

(define-syntax test-single-element-list
  (syntax-rules ()
    ((_ (x)) x)))

(define-syntax test-deeply-nested
  (syntax-rules ()
    ((_ (((x) ...) ...)) (quote (((x) ...) ...)))))

;; === MULTIPLE RULES ===
(define-syntax test-multi-rule
  (syntax-rules ()
    ((_) (quote empty))
    ((_ x) (quote (single x)))
    ((_ x y) (quote (pair x y)))
    ((_ x (y ...)) (quote (multi x (y ...))))))

;; === PATTERN VARIABLE SHADOWING (same var at different depths in different rules) ===
(define-syntax test-var-shadowing
  (syntax-rules ()
    ((_ x) (quote (depth-0 x)))
    ((_ (x ...)) (quote (depth-1 x ...)))))

;; === CONSTANT PATTERNS ===
(define-syntax test-constant-number
  (syntax-rules ()
    ((_ 42 x) (quote (found-42 x)))))

(define-syntax test-constant-string
  (syntax-rules ()
    ((_ "hello" x) (quote (found-hello x)))))

;; === BARE VARIABLES IN ELLIPSIS CONTEXT (Critical test for R7RS compliance) ===
;; These patterns should work if the compiler properly tracks depths
(define-syntax test-bare-var-ellipsis
  (syntax-rules ()
    ((_ (x) ...) (quote ((x) ...)))))

(define-syntax test-nested-bare-var
  (syntax-rules ()
    ((_ ((x)) ...) (quote (((x)) ...)))))

;; Now actual invocations to test matching

;; Simple tests
(test-simple-var 42)
(test-simple-multi a b c)

;; Ellipsis tests  
(test-ellipsis-basic 1 2 3 4 5)
(test-ellipsis-basic)  ;; empty ellipsis
(test-ellipsis-pair (x 1) (y 2) (z 3))
(test-ellipsis-pair)  ;; empty

;; Nested ellipsis tests
(test-nested-simple ((1 2 3) (4 5) (6)))
(test-nested-simple ())  ;; empty outer
(test-nested-simple (())) ;; empty inner
(test-nested-pair ((a 1) (b 2)) ((c 3)))

;; Mixed depth tests
(test-mixed-before prefix 1 2 3)
(test-mixed-before prefix)

;; Complex patterns
(test-let-like ((x 1) (y 2) (z 3)) body-expr)
(test-let-like () body-expr)

;; Literal matching
(test-literal (> 5 3) => result)
(test-multiple-literals lambda foo)
(test-multiple-literals let bar)
(test-multiple-literals if baz)

;; Wildcard matching  
(test-wildcard ignore 42 also-ignore)
(test-wildcard-ellipsis 1 2 3 4 5)
(test-wildcard-ellipsis)

;; Edge cases
(test-single-element-list (only-one))
(test-deeply-nested (((1) (2)) ((3))))

;; Multiple rules
(test-multi-rule)
(test-multi-rule single-arg)
(test-multi-rule first second)
(test-multi-rule first (a b c))

;; Variable shadowing
(test-var-shadowing solo)
(test-var-shadowing (a b c))

;; Constants
(test-constant-number 42 matched)
(test-constant-string "hello" matched)

;; Bare variables - critical R7RS compliance tests
(test-bare-var-ellipsis (1) (2) (3))
(test-bare-var-ellipsis)
(test-nested-bare-var ((a)) ((b)) ((c)))
(test-nested-bare-var)
