#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide ticks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (ticks thk)
  (define (measure)
    (define before (current-memory-use 'cumulative))
    (thk)
    (define after (current-memory-use 'cumulative))
    (- after before))
  (collect-garbage 'major)
  (define measurements (list (measure) (measure) (measure) (measure) (measure)))
  (/ (list-ref (sort measurements <) 2) 1000))
