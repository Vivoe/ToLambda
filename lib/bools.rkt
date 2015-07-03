#lang racket

(require "utils.rkt")
(require "lambdaFunctions.rkt")

(provide ls-and ls-not ls-or ls-bool)

(define (ls-bool b)
  (if b ls-true ls-false))

(define ls-and 
  (string-append 
   (ls-lambda "a" (ls-lambda "b" (ls-apply (ls-apply"a" "b") ls-false)))))

(define ls-not
  (string-append (ls-lambda "x" (ls-apply (ls-apply "x" ls-false) ls-true))))

(define ls-or
  (string-append (ls-lambda "a" (ls-lambda "b" (ls-apply (ls-apply "a" ls-true) "b")))))


(define (tests group)
  (match group
    ['eval
     (println "eval")
     (println (eval-bool (run-string ls-true))) ;=> true
     (println (eval-bool (run-string ls-false))) ;=> false
     ]
    ['and
     (println "and")
     (println (eval-bool (run-string (ls-apply ls-and ls-true ls-true)))) ;=> true
     (println (eval-bool (run-string (ls-apply ls-and ls-false ls-true)))) ;=> false
     (println (eval-bool (run-string (ls-apply ls-and ls-true ls-false)))) ;=> false
     (println (eval-bool (run-string (ls-apply ls-and ls-false ls-false)))) ;=> false
     ]
    ['not
     (println "not")
     (println (eval-bool (run-string (ls-apply ls-not ls-true)))) ;=> false
     (println (eval-bool (run-string (ls-apply ls-not ls-false)))) ;=> true
     ]
    ['or
     (println "or")
     (println (eval-bool (run-string (ls-apply ls-or ls-true ls-true)))) ;=> true
     (println (eval-bool (run-string (ls-apply ls-or ls-false ls-true)))) ;=> true
     (println (eval-bool (run-string (ls-apply ls-or ls-true ls-false)))) ;=> true
     (println (eval-bool (run-string (ls-apply ls-or ls-false ls-false)))) ;=> false
     ]
    ))