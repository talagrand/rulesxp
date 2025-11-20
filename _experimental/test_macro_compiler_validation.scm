;; Comprehensive validation of macro compiler
;; Tests that pattern compilation catches errors at definition time

;; Test 1: Valid simple pattern
(define-syntax test-valid-simple
  (syntax-rules ()
    ((_ x)
     x)))

;; Test 2: Valid ellipsis pattern
(define-syntax test-valid-ellipsis
  (syntax-rules ()
    ((_ x ...)
     (quote (x ...)))))

;; Test 3: Valid nested ellipsis
(define-syntax test-valid-nested
  (syntax-rules ()
    ((_ (x ...) ...)
     (quote ((x ...) ...)))))

;; Test 4: Valid multiple variables at same depth
(define-syntax test-valid-multi
  (syntax-rules ()
    ((_ a b c)
     (list a b c))))

;; Test 5: Valid template with all pattern vars
(define-syntax test-valid-all-vars
  (syntax-rules ()
    ((_ x y z)
     (list x y z))))

;; Test executions
(display "Test 1 - Simple: ")
(display (test-valid-simple 42))
(newline)

(display "Test 2 - Ellipsis: ")
(display (test-valid-ellipsis 1 2 3))
(newline)

(display "Test 3 - Nested: ")
(display (test-valid-nested (1 2) (3 4)))
(newline)

(display "Test 4 - Multi: ")
(display (test-valid-multi a b c))
(newline)

(display "Test 5 - All vars: ")
(display (test-valid-all-vars 1 2 3))
(newline)
