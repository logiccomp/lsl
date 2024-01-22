#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/provide
         racket/math
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (lift-out
  random
  sqr
  sqrt
  ; TODO: Verify all definitions below behave like ISL.
  acos
  angle
  asin
  atan
  complex?
  conjugate
  cos
  cosh
  current-seconds
  denominator
  ; TODO: e
  exact?
  exp
  gcd
  imag-part
  inexact?
  integer->char
  integer-sqrt
  lcm
  log
  magnitude
  make-polar
  make-rectangular
  ; TODO: number->string-digits
  numerator
  rational?
  real-part
  round
  sin
  sinh
  tan)

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
   ^integer?
   ^max
   ^min
   ^modulo
   ^negative?
   ^number?
   ^odd?
   ^pi
   ^positive?
   ^quotient
   ^real?
   ^remainder
   ^sgn
   ^sub1
   ^zero?)))
