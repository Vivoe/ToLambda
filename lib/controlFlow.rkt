#lang racket

(require "utils.rkt")
(require "lambdaFunctions.rkt")

(provide ls-if)

(define (ls-if condi tex fex)
  (ls-apply condi tex fex))

(define (ls-func var f-body use-f)
  (ls-apply (string-append "(λ (" var ") " use-f ")") f-body))


(define (tests group)
  (match group
    ['if
     (println "if")
     (println (eval-bool (run-string (ls-if ls-true ls-true ls-false)))) ;=>true
     (println (eval-bool (run-string (ls-if ls-false ls-true ls-false)))) ;=>false
     ]))