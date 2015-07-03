#lang racket

(require "lib/utils.rkt")
(require "lib/lambdaFunctions.rkt")
(require "lib/bools.rkt")
(require "lib/numerics.rkt")
(require "lib/controlFlow.rkt")

(require "compile.rkt")

;To run things, (eval-num (run-string stuff))
;Ex:
;(eval-rat (run-string (ls-if (ls-apply ls-and ls-true ls-false)
;                               (ls-add (ls-num 3) (ls-num (/ 3 4)))
;                               (ls-sub (ls-mult (ls-num (/ 3 2)) (ls-num (/ 5 2)))
;                                                (ls-num (/ 10 4))))))
;=>5/4
;NOTE FOR RUNNING THE CODE: RUN STRING ONLY WORKS IN INTERACTIONS WINDOW

;To get the actual lambda calulus code, do not include eval-num and run-string.
;Write to file example
;(define out (open-output-file "output.txt" #:mode 'text #:exists 'replace))
;(display (ls-if (ls-apply ls-and ls-true ls-false)
;                (ls-add (ls-num 3) (ls-num (/ 3 4)))
;                (ls-sub (ls-mult (ls-num (/ 3 2)) (ls-num (/ 5 2)))
;                        (ls-num (/ 10 4)))) out)
;(close-output-port out)

(define (toFile prog)
  (define out (open-output-file "output.txt" #:mode 'text #:exists 'replace))
  (display prog out)
  (close-output-port out))

(define (numToOutput)
  (eval-num (run-string (compile-to-lambda))))