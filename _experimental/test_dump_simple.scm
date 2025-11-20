;; Simple test to verify dump output

;; Test 1: Simple pattern
(define-syntax test-simple
  (syntax-rules ()
    ((_ x) x)))

;; Test 2: Single ellipsis
(define-syntax test-ellipsis
  (syntax-rules ()
    ((_ x ...) (quote (x ...)))))

;; Test 3: Nested ellipsis
(define-syntax test-nested
  (syntax-rules ()
    ((_ (x ...) ...) (quote ((x ...) ...)))))

;; Test 4: Let-style
(define-syntax test-let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))
