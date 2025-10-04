(begin
  (define square (lambda (x) (* x x)))
  (define multiply-add (lambda (a b c) (+ (* a b) c)))
  (+ (square 10) (multiply-add 7 6 5)))