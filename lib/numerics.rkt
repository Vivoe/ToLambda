#lang racket

(require "utils.rkt")
(require "lambdaFunctions.rkt")

(provide(rename-out [toRational ls-num]
                    [add-rat ls-add]
                    [sub-rat ls-sub]
                    [mult-rat ls-mult]
                    [div-rat ls-div]))

;PLEASE DON'T INPUT DECIMALS
;Outputs all numbers as rational representations.

;Number definitions and printing
;Naturals represented as unary
;Integers as pairs of naturals
;Rationals as pairs of Integers.
(define (toNatural n)
  (string-append "(λ (s) (λ (z) "
                 (repeat-string n "(s ")
                 "z"
                 (make-string n #\))
                 "))"))

(define (toInteger n)
  (if (>= n 0)
      (ls-pair (toNatural n) (toNatural 0))
      (ls-pair (toNatural 0) (toNatural (- n)))))

(define (toRational n)
  (let ([num (numerator n)]
        [denom (denominator n)])
    (ls-pair (toInteger num) (toInteger denom))))

;Numeric operations
(define l-succ (λ (w) (λ (y) (λ (x) (y ((w y) x))))))
(define ls-succ "(λ (w) (λ (y) (λ (x) (y ((w y) x)))))")

  
;NATURALS AND INTEGERS
;Addition
(define (add-nat a b)
  (ls-apply (ls-apply a ls-succ) b))

(define (add-int a b)
  (ls-pair (add-nat (ls-pair-fst a) (ls-pair-fst b))
           (add-nat (ls-pair-snd a) (ls-pair-snd b))))

(define (add-rat a b)
  (ls-pair (add-int (mult-int (ls-pair-fst a) (ls-pair-snd b))
                    (mult-int (ls-pair-snd a) (ls-pair-fst b)))
           (mult-int (ls-pair-snd a) (ls-pair-snd b))))

;subtraction
(define (sub-int a b)
  (ls-pair (add-nat (ls-pair-fst a) (ls-pair-snd b))
           (add-nat (ls-pair-snd a) (ls-pair-fst b))))

(define (sub-rat a b)
  (ls-pair (sub-int (mult-int (ls-pair-fst a) (ls-pair-snd b))
                    (mult-int (ls-pair-snd a) (ls-pair-fst b)))
           (mult-int (ls-pair-snd a) (ls-pair-snd b))))

;Multiplication
(define (mult-nat a b)
  (ls-apply (ls-apply a (ls-apply b ls-succ)) (toNatural 0)))

(define (mult-int a b)
  (ls-pair (add-nat (mult-nat (ls-pair-fst a) (ls-pair-fst b))
                    (mult-nat (ls-pair-snd a) (ls-pair-snd b)))
           (add-nat (mult-nat (ls-pair-fst a) (ls-pair-snd b))
                    (mult-nat (ls-pair-snd a) (ls-pair-fst b)))))

(define (mult-rat a b)
  (ls-pair (mult-int (ls-pair-fst a) (ls-pair-fst b))
           (mult-int (ls-pair-snd a) (ls-pair-snd b))))

;division
(define (div-rat a b)
  (ls-pair (mult-int (ls-pair-fst a) (ls-pair-snd b))
           (mult-int (ls-pair-snd a) (ls-pair-fst b))))
  
  ;tests
  ;I would use check-expect but it doesn't work ):
  ;Also, run-string doesn't like working in the actual code.
  ;Run (tests) in the interactions window.
  (define (tests group)
    (cond
      [(equal? 'reps group)
       (println "Representations")
       (println (eval-nat (run-string (toNatural 10)))) ;=> n, n in N
       (println (eval-nat (run-string (toNatural 0)))) ;=> 0
       (println (eval-int (run-string (toInteger 10)))) ;=> n, n in Z. n > 0
       (println (eval-int (run-string (toInteger -10)))) ;=> n, n in Z. n < 0
       (println (eval-int (run-string (toInteger 0)))) ;=> 0, n in Z. n = 0
       
       (println (eval-rat (run-string (toRational (/ 3 2))))) ;=> 3/2, n in Q
       (println (eval-rat (run-string (toRational (/ 3 -1))))) ;=> -3, n in Q
       (println (eval-rat (run-string (toRational 1)))) ;=> 1
       (println (eval-rat (run-string (toRational 0)))) ;=> 0
       ]
      [(equal? 'misc group)
       (println "Misc")
       (println "Succ")
       (println (eval-nat (run-string (ls-apply ls-succ (toNatural 1))))) ;=> 2
       (println (eval-nat (run-string (ls-apply ls-succ (toNatural 0))))) ;=> 1
       ]
      [(equal? 'add group)
       (println "add")
       (println "nats")
       (println (eval-nat (run-string (add-nat (toNatural 2) (toNatural 3))))) ;=> 5
       (println (eval-nat (run-string (add-nat (toNatural 2) (toNatural 0))))) ;=> 2
       (println (eval-nat (run-string (add-nat (toNatural 0) (toNatural 2))))) ;=> 2
       (println (eval-nat (run-string (add-nat (toNatural 0) (toNatural 0))))) ;=> 0
       (println "ints")
       (println (eval-int (run-string (add-int (toInteger 1) (toInteger 3))))) ;=> 4
       (println (eval-int (run-string (add-int (toInteger 0) (toInteger 3))))) ;=> 3
       (println (eval-int (run-string (add-int (toInteger 0) (toInteger -3))))) ;=> -3
       (println (eval-int (run-string (add-int (toInteger -1) (toInteger 3))))) ;=> 2
       (println (eval-int (run-string (add-int (toInteger -1) (toInteger -3))))) ;=> -4
       (println (eval-int (run-string (add-int (toInteger -1) (toInteger 1))))) ;=> 0
       (println (eval-int (run-string (add-int (toInteger 0) (toInteger 0))))) ;=> 0
       (println "rats")
       (println (eval-rat (run-string (add-rat (toRational (/ 3 2)) (toRational (/ 5 3)))))) ;=> 19/6
       (println (eval-rat (run-string (add-rat (toRational (/ 3 2)) (toRational 0))))) ;=> 3/2
       (println (eval-rat (run-string (add-rat (toRational (/ 3 2)) (toRational (/ -2 5)))))) ;=> 11/10
       (println (eval-rat (run-string (add-rat (toRational (/ -3 2)) (toRational (/ -2 5)))))) ;=> -19/10
       ]
      [(equal? 'sub group)
       (print "sub")
       (println "ints")
       (println (eval-int (run-string (sub-int (toInteger 1) (toInteger 3))))) ;=> -2
       (println (eval-int (run-string (sub-int (toInteger 0) (toInteger 3))))) ;=> -3
       (println (eval-int (run-string (sub-int (toInteger 0) (toInteger -3))))) ;=> 3
       (println (eval-int (run-string (sub-int (toInteger -1) (toInteger 3))))) ;=> -4
       (println (eval-int (run-string (sub-int (toInteger -1) (toInteger -3))))) ;=> 2
       (println "rats")
       (println (eval-rat (run-string (sub-rat (toRational (/ 3 2)) (toRational (/ 5 3)))))) ;=> -1/6
       (println (eval-rat (run-string (sub-rat (toRational (/ 3 2)) (toRational 0))))) ;=> 3/2
       (println (eval-rat (run-string (sub-rat (toRational 0) (toRational (/ 3 2)))))) ;=> -3/2
       (println (eval-rat (run-string (sub-rat (toRational (/ 3 2)) (toRational (/ 3 2)))))) ;=> 0
       (println (eval-rat (run-string (sub-rat (toRational (/ 3 2)) (toRational (/ -2 5)))))) ;=> 19/10
       (println (eval-rat (run-string (sub-rat (toRational (/ -3 2)) (toRational (/ -2 5)))))) ;=> -11/10
       ]
      [(equal? 'mult group)
       (println "mult")
       (println "nats")
       (println (eval-nat (run-string (mult-nat (toNatural 2) (toNatural 3))))) ;=> 6
       (println (eval-nat (run-string (mult-nat (toNatural 2) (toNatural 0))))) ;=> 0
       (println (eval-nat (run-string (mult-nat (toNatural 0) (toNatural 3))))) ;=> 0
       (println (eval-nat (run-string (mult-nat (toNatural 0) (toNatural 0))))) ;=> 0
       (println "ints")
       (println (eval-int (run-string (mult-int (toInteger 2) (toInteger 3))))) ;=> 6
       (println (eval-int (run-string (mult-int (toInteger 2) (toInteger -3))))) ;=> -6
       (println (eval-int (run-string (mult-int (toInteger 0) (toInteger 0))))) ;=> 0
       (println (eval-int (run-string (mult-int (toInteger -3) (toInteger -3))))) ;=> 9
       (println "rats")
       (println (eval-rat (run-string (mult-rat (toRational (/ 3 2)) (toRational (/ 4 2)))))) ;=> 3
       (println (eval-rat (run-string (mult-rat (toRational (/ 3 2)) (toRational (/ 4 -2)))))) ;=> -3
       (println (eval-rat (run-string (mult-rat (toRational (/ 3 2)) (toRational 1))))) ;=> 3/2
       (println (eval-rat (run-string (mult-rat (toRational (/ 3 2)) (toRational 0))))) ;=> 0
       ]
      [(equal? 'div group)
       (println "div")
       (println "rat")
       (println (eval-rat (run-string (div-rat (toRational (/ 3 2)) (toRational 2))))) ;=>3/4
       (println (eval-rat (run-string (div-rat (toRational (/ 3 2)) (toRational -2))))) ;=>-3/4
       ]
      ))
  
  
  