#lang racket

(require "lib/utils.rkt")
(require "lib/lambdaFunctions.rkt")
(require "lib/bools.rkt")
(require "lib/numerics.rkt")
(require "lib/controlFlow.rkt")

;To run things, (eval-rat (run-string stuff))
;Ex:
;(eval-rat (run-string (ls-if (ls-apply ls-and ls-true ls-false)
;                               (ls-add (ls-num 3) (ls-num (/ 3 4)))
;                               (ls-sub (ls-mult (ls-num (/ 3 2)) (ls-num (/ 5 2)))
;                                                (ls-num (/ 10 4))))))
;=>5/4