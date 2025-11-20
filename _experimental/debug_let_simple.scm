;; Test the let macro which uses ellipsis in lambda parameters

(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; This should expand to ((lambda (x y) (+ x y)) 1 2)
(let ((x 1) (y 2))
  (+ x y))
