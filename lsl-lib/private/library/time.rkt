#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide distinguishable?
         measure
         ticks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require t-test
         racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define SAMPLES 500)
(define LEVEL 0.0001)

(define ticks (make-parameter 0))

;; are the number of character comparisons (via string=?) in the given thunks
;; distinguishable?
(define (distinguishable? thk1 thk2)
  (ticks 0)

  (thk1)
  (define ticks1 (ticks))
  (ticks 0)

  (thk2)
  (define ticks2 (ticks))
  (ticks 0)

  (not (= ticks1 ticks2)))

(define (measure thk)
  (define-values (_results cpu _real _gc)
    (time-apply thk '()))
  cpu)
