#lang racket

(provide make-var 
         repeat-string
         stringify
         run-string
         println)

(define (make-var n)
  (string-append "x_" (number->string n)))

(define (repeat-string n str)
  (cond
    [(zero? n) ""]
    [else (string-append str (repeat-string (sub1 n) str))]))

(define (stringify lst)
  (apply string-append lst))

(define (run-string str)
  (eval (read (open-input-string str))))

(define (println x)
  (display x) (newline))

(define (nd a)
  (println (numerator a))
  (println (denominator a)))