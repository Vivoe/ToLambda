#lang racket

(require racket/trace)

(define prog (open-input-file "lambdaLangTest.rkt"))

(struct program (prog) #:mutable)

(define (a str)
  (set! prog (append prog str)))

;(define (run)
;  (define exp (read prog))
;  (cond
;    [(eof-object? exp)  ((λ (x) x) "end")]
;    [else (print (eval exp)) (newline) (run)]))

;(define (eval exp)
;  (match exp
;    [`(+ ,x ,y) (+ (eval x) (eval y))]
;    [`(* ,x ,y) (* (eval x) (eval y))]
;    [x x]))

(define (compile)
  (define exp (read prog))
  (define prog (program))
  (define (a str) (set! prog (append prog str)))
  (cond
    [(eof-object? exp) (print prog)]
    [else (eval exp) (compile)]))

(define (eval exp)
  (match exp
    [(? number? x) (toNumber x)]))

(define (toNumber x)
  (cond
    [(and (integer? x) (x > 0))

;(run)
