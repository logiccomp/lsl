#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/contract
         racket/provide
         racket/math
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [rename ^natural? natural? (-> any? ^boolean?)]
  [rename ^integer? integer? (-> any? ^boolean?)]
  [rename ^number? number? (-> any? ^boolean?)]
  [rename ^real? real? (-> any? ^boolean?)])

 (lift-out
  random
  sqr
  sqrt)

 (filtered-out
  (strip "^")
  (combine-out
   ^*
   ^+
   ^-
   ^/
   ^<
   ^<=
   ^=
   ^>
   ^>=
   ^abs
   ^add1
   ^ceiling
   ^even?
   ^exact->inexact
   ^expt
   ^floor
   ^inexact->exact
   ^max
   ^min
   ^modulo
   ^negative?
   ^odd?
   ^pi
   ^positive?
   ^quotient
   ^remainder
   ^sgn
   ^sub1
   ^zero?)))

(define (^natural? x)
  (^and (^integer? x) (^>= x 0)))
