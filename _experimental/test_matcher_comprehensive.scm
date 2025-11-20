;; Comprehensive matcher test suite - stress testing all pattern types

;; === DEPTH 0: Simple direct bindings ===
(define-syntax test-simple-var
  (syntax-rules ()
    ((_ x) x)))

(define-syntax test-simple-multi
  (syntax-rules ()
    ((_ x y z) (list x y z))))

;; === DEPTH 1: Single ellipsis ===
(define-syntax test-ellipsis-basic
  (syntax-rules ()
    ((_ x ...) (list x ...))))

(define-syntax test-ellipsis-multi-var
  (syntax-rules ()
    ((_ (name val) ...) (list (list name val) ...))))

(define-syntax test-ellipsis-nested-pattern
  (syntax-rules ()
    ((_ ((a b) ...)) (list (list a b) ...))))

;; === DEPTH 2: Nested ellipsis ===
(define-syntax test-nested-simple
  (syntax-rules ()
    ((_ ((x ...) ...)) (list (list x ...) ...))))

(define-syntax test-nested-multi-var
  (syntax-rules ()
    ((_ ((a b) ...) ...) (list (list a b ...) ...))))

(define-syntax test-triple-nested
  (syntax-rules ()
    ((_ (((x ...) ...) ...)) (list (list (list x ...) ...) ...))))

;; === MIXED DEPTHS: Regular + ellipsis ===
(define-syntax test-mixed-before
  (syntax-rules ()
    ((_ prefix x ...) (list prefix x ...))))

(define-syntax test-mixed-middle
  (syntax-rules ()
    ((_ (x ...) separator (y ...)) (list (list x ...) separator (list y ...)))))

;; === COMPLEX PATTERNS ===
(define-syntax test-let-like
  (syntax-rules ()
    ((_ ((var val) ...) body ...) 
     ((lambda (var ...) body ...) val ...))))

(define-syntax test-cond-like
  (syntax-rules ()
    ((_ (test expr ...) ...)
     (list (list test expr ...) ...))))

;; === LITERALS ===
(define-syntax test-literal
  (syntax-rules (=>)
    ((_ test => result) (if test result #f))))

;; === WILDCARD ===
(define-syntax test-wildcard
  (syntax-rules ()
    ((_ _ x _) x)))

;; === EDGE CASES ===
(define-syntax test-empty-ellipsis
  (syntax-rules ()
    ((_ x ...) (list x ...))))

(define-syntax test-nested-empty
  (syntax-rules ()
    ((_ ((x ...) ...)) (list (list x ...) ...))))

;; === REAL-WORLD PATTERNS ===
(define-syntax test-and
  (syntax-rules ()
    ((_) #t)
    ((_ test) test)
    ((_ test rest ...) (if test (test-and rest ...) #f))))

(define-syntax test-let*
  (syntax-rules ()
    ((_ () body ...) (begin body ...))
    ((_ ((var val) rest ...) body ...)
     ((lambda (var) (test-let* (rest ...) body ...)) val))))

;; Now actual invocations to test matching
;; These will fail expansion but should succeed matching

;; Simple tests
(test-simple-var 42)
(test-simple-multi a b c)

;; Ellipsis tests  
(test-ellipsis-basic 1 2 3 4 5)
(test-ellipsis-basic)  ;; empty ellipsis
(test-ellipsis-multi-var (x 1) (y 2) (z 3))
(test-ellipsis-multi-var)  ;; empty

;; Nested ellipsis tests
(test-nested-simple ((1 2 3) (4 5) (6)))
(test-nested-simple ())  ;; empty outer
(test-nested-simple (())) ;; empty inner
(test-nested-multi-var ((a 1) (b 2)) ((c 3)))

;; Triple nested - extreme test
(test-triple-nested ((((1 2) (3 4)) ((5 6))) (((7)))))

;; Mixed depth tests
(test-mixed-before prefix 1 2 3)
(test-mixed-before prefix)

;; Complex real-world patterns
(test-let-like ((x 1) (y 2) (z 3)) (+ x y z))
(test-let-like () (display "no bindings"))

(test-cond-like (#t 1) (#f 2) (else 3))
(test-cond-like)  ;; empty cond

;; Literal matching
(test-literal (> 5 3) => "yes")

;; Wildcard matching  
(test-wildcard ignore 42 also-ignore)

;; And pattern with various lengths
(test-and)
(test-and #t)
(test-and #t #t #t)

;; Let* pattern
(test-let* ((x 1) (y 2)) (+ x y))
(test-let* () (display "empty"))
