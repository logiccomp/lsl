#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide ticks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (ticks thk)
  (define (measure)
    (define before (current-milliseconds))
    (thk)
    (define after (current-milliseconds))
    (- after before))
  (collect-garbage 'major)
  (define measurements (list (measure) (measure) (measure)))
  (list-ref (sort measurements <) 1))
