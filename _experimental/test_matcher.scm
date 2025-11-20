;; Test cases for macro_matcher.rs validation

;; Test 1: Simple direct binding (depth 0)
(define-syntax test-simple
  (syntax-rules ()
    ((_ x)
     x)))

;; Test 2: Single ellipsis (depth 1)
(define-syntax test-ellipsis
  (syntax-rules ()
    ((_ (x ...))
     (list x ...))))

;; Test 3: Nested ellipsis (depth 2)
(define-syntax test-nested
  (syntax-rules ()
    ((_ ((x ...) ...))
     (list (list x ...) ...))))

;; Test 4: Multiple variables at depth 1
(define-syntax test-multi
  (syntax-rules ()
    ((_ (name val) ...)
     (list name ... val ...))))

;; Now test actual matching
(test-simple 42)
(test-ellipsis (1 2 3))
(test-nested ((a b) (c d e)))
(test-multi (x 1) (y 2) (z 3))
