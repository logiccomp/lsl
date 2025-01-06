#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide distinguishable?
         measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require t-test
         racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define SAMPLES 1000)
(define LEVEL 0.0001)

(define (distinguishable? thk1 thk2)
  (collect-garbage 'major)
  (define thk1-vals (map (λ _ (measure thk1)) (range SAMPLES)))
  (collect-garbage 'major)
  (define thk2-vals (map (λ _ (measure thk2)) (range SAMPLES)))
  (when (and (= (length (remove-duplicates thk1-vals)) 1)
             (= (length (remove-duplicates thk2-vals)) 1))
    (error 'distinguishable? "not enough variance to detect difference (try larger input sizes)"))
  (< (student-t-test thk1-vals thk2-vals) LEVEL))

(define (measure thk)
  (define-values (_results cpu _real _gc)
    (time-apply thk '()))
  cpu)
