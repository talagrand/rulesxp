;; Debug test for lambda parameter expansion issue

(define-syntax basic-empty
  (syntax-rules ()
    ((_ (x ...))
     (quote (x ...)))))

;; This should expand to (quote ())
(display (basic-empty ()))
(newline)
