;; Test to see what happens with partial CPS transformation
;; Start with a simple nested expression that might cause partial failure
(+ (* 2 3) (* 4 5) (* 6 7) (* 8 9))