;; ===== SAMPLESCHEME FUNCTION PRELUDE =====
;; Core Scheme functions implemented in Scheme itself

;; ===== TYPE PREDICATES =====
;; Note: Type predicates null? and list? are builtins
;; null? checks for empty list: '()
;; list? checks if value is a list (empty or non-empty)
;; pair? checks for non-empty list (cons cell) - implemented as (list? x) AND NOT (null? x)

(define (pair? x)
  (if (list? x)
      (not (null? x))
      #f))

;; ===== LIST ACCESSOR COMBINATORS =====
;; R7RS specifies all 30 combinations of car/cdr up to 4 levels.
;; These are implemented compositionally for clarity.

;; Level 2
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Level 3
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

;; Level 4
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))

;; ===== LIST UTILITIES =====
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

;; Simple recursive reverse without inner defines
(define (reverse lst)
  ($reverse-impl lst '()))

(define ($reverse-impl lst acc)
  (if (null? lst)
      acc
      ($reverse-impl (cdr lst) (cons (car lst) acc))))

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (list-tail lst n)
  (if (= n 0)
      lst
      (list-tail (cdr lst) (- n 1))))

(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

;; ===== HIGHER-ORDER FUNCTIONS =====
;; R7RS variadic map: (map proc list1 list2 ...) applies proc to corresponding elements
;; Using case-lambda to handle both single-list and multi-list cases
(define map
  (case-lambda
    ((proc lst)
     (if (null? lst)
         '()
         (cons (proc (car lst)) (map proc (cdr lst)))))
    ((proc lst1 lst2)
     (if (null? lst1)
         '()
         (cons (proc (car lst1) (car lst2))
               (map proc (cdr lst1) (cdr lst2)))))
    ((proc lst1 lst2 lst3)
     (if (null? lst1)
         '()
         (cons (proc (car lst1) (car lst2) (car lst3))
               (map proc (cdr lst1) (cdr lst2) (cdr lst3)))))))

(define (filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))



(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))

(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (fold-right proc init (cdr lst)))))

(define (for-each proc lst)
  (if (not (null? lst))
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

(define (any pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (any pred (cdr lst)))))

(define (every pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every pred (cdr lst))
          #f)))

;; ===== MATHEMATICAL OPERATIONS =====
(define (abs x) (if (< x 0) (- x) x))
(define (max x y) (if (> x y) x y))
(define (min x y) (if (< x y) x y))

;; **R7RS RESTRICTED:** `even?` and `odd?` require the `modulo` or `remainder`
;; primitive, which is not implemented.
;; (define (even? n) (= (modulo n 2) 0))
;; (define (odd? n) (not (even? n)))

(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))

;; ===== LIST SEARCH OPERATIONS =====
(define (member obj lst)
  (if (null? lst)
      #f
      (if (equal? obj (car lst))
          lst
          (member obj (cdr lst)))))

(define (assoc key alist)
  (if (null? alist)
      #f
      (if (equal? key (caar alist))
          (car alist)
          (assoc key (cdr alist)))))

;; ===== COMBINATORS =====
(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

(define (uncurry f)
  (lambda (x y) ((f x) y)))

(define (flip f)
  (lambda (x y) (f y x)))

;; ===== ENVIRONMENT UTILITIES =====
(define (void) (if #f #f))

(define (newline)
  (display "\n"))

;; ===== APPLY =====
;; R7RS apply: (apply proc arg1 ... args)
;; The last argument must be a list, and proc is called with all args unpacked
;; **R7RS RESTRICTED:** Supports up to 5 args. Full apply requires builtin support.
(define (apply proc args)
  (if (null? args)
      (proc)
      (if (null? (cdr args))
          (proc (car args))
          (if (null? (cdr (cdr args)))
              (proc (car args) (car (cdr args)))
              (if (null? (cdr (cdr (cdr args))))
                  (proc (car args) (car (cdr args)) (car (cdr (cdr args))))
                  (if (null? (cdr (cdr (cdr (cdr args)))))
                      (proc (car args) (car (cdr args)) (car (cdr (cdr args))) (car (cdr (cdr (cdr args)))))
                      (if (null? (cdr (cdr (cdr (cdr (cdr args))))))
                          (proc (car args) (car (cdr args)) (car (cdr (cdr args))) (car (cdr (cdr (cdr args)))) (car (cdr (cdr (cdr (cdr args))))))
                          (error "apply: too many arguments - only supports up to 5 args"))))))))