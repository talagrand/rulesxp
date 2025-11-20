;; Simple test for nested ellipsis in template

(define-syntax test-nested-ellipsis
  (syntax-rules ()
    ((test-nested-ellipsis ((x y) ...))
     ((x y) ...))))

(test-nested-ellipsis ((1 2) (3 4)))
