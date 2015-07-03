#lang racket

(require "utils.rkt")

(provide ls-apply
         ls-pair
         l-identity ls-identity
         ls-pair-fst ls-pair-snd
         l-true ls-true
         l-false ls-false
         ls-lambda
         eval-bool eval-nat eval-int eval-rat
         )

;l is the lambda representation (code)
;ls is the lambda string representation.

;a :: string, b :: string
;(define (ls-apply a b)
  ;(string-append "(" a " " b ")"))

(define (ls-apply func . args)
  (string-append (make-string (length args) #\()
                 func
                 " "
                 (foldr (λ (arg acc) (string-append " " arg ")" acc))
                        "" args)))

;a :: string, b :: string
(define (ls-pair a b)
  (string-append "(λ (z) ((z " a ") " b "))"))

(define l-identity (λ (x) x))
(define ls-identity "(λ (x) x)")

(define (ls-pair-fst p)
  (ls-apply p ls-true))

(define (ls-pair-snd p)
  (ls-apply p ls-false))

(define (ls-lambda var body)
  (string-append "(λ (" var ") " body ")"))

(define l-true (λ (x) (λ (y) x)))
(define ls-true "(λ (x) (λ (y) x))")

(define l-false (λ (x) (λ (y) y)))
(define ls-false "(λ (x) (λ (y) y))")

;Evals
(define (eval-bool l)
  ((l true) false))

(define (eval-nat l)
  ((l add1) 0))

(define (eval-int l)
  (- (eval-nat (l l-true)) (eval-nat (l l-false))))

(define (eval-rat l)
  (/ (eval-int (l l-true)) (eval-int (l l-false))))