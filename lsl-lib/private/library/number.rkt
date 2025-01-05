#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/contract
         racket/provide
         racket/math
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [natural? (-> any? boolean?)]
  [integer? (-> any? boolean?)]
  [number? (-> any? boolean?)]
  [real? (-> any? boolean?)])

 random
 sqr
 sqrt
 *
 +
 -
 /
 <
 <=
 =
 >
 >=
 abs
 add1
 ceiling
 even?
 exact->inexact
 expt
 floor
 inexact->exact
 max
 min
 modulo
 negative?
 odd?
 pi
 positive?
 quotient
 remainder
 sgn
 sub1
 zero?)

(define (natural? x)
  (and (integer? x) (>= x 0)))
