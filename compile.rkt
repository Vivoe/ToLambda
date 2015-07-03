#lang racket

(require "lib/utils.rkt")
(require "lib/lambdaFunctions.rkt")
(require "lib/bools.rkt")
(require "lib/numerics.rkt")
(require "lib/controlFlow.rkt")

(provide compile-to-lambda)

(define (compile-to-lambda)
  (define in (open-input-file "input.txt"))
  (define prog (read in))
  (close-input-port in)
  (eval prog))

(define (eval prog)
  (match prog
    [`(+ ,x ,y) (ls-add (eval x) (eval y))]
    [`(- ,x ,y) (ls-sub (eval x) (eval y))]
    [`(* ,x ,y) (ls-mult (eval x) (eval y))]
    [`(/ ,x ,y) (ls-div (eval x) (eval y))]
    [`(and ,x ,y) (ls-apply ls-and (eval x) (eval y))]
    [`(or ,x ,y) (ls-apply ls-or (eval x) (eval y))]
    [`(not ,x) (ls-apply ls-not (eval x))]
    [`(if ,condi ,tex ,fex) (ls-if (eval condi) (eval tex) (eval fex))]
    [(? number? x) (ls-num x)]
    ['true (ls-bool #t)];The better way didn't work ):
    ['false (ls-bool #f)]
    [x (println x) (error "Unsupported construct!")]))