; Minimal test for optional ellipsis bug
; Pattern: ((a b ...) ...)  where b can be present or absent

(define-syntax test-macro
  (syntax-rules ()
    ((_ ((a b ...) ...))
     (list (list a b ...) ...))))

; Test 1: Both have values
(test-macro ((x 1 2) (y 3 4)))
; Should expand to: (list (list x 1 2) (list y 3 4))

; Test 2: Second has no b values  
(test-macro ((x 1 2) (y)))
; Should expand to: (list (list x 1 2) (list y))

; Test 3: Neither has b values
(test-macro ((x) (y)))
; Should expand to: (list (list x) (list y))
