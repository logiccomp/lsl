#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide distinguishable?
         measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require t-test
         math/statistics
         racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define SAMPLES 1000)
(define LEVEL 0.000001)

(define (distinguishable? thk1 thk2)
  (collect-garbage 'major)
  (define thk1-vals (clamp-to-iqr (map (λ _ (measure thk1)) (range SAMPLES))))
  (collect-garbage 'major)
  (define thk2-vals (clamp-to-iqr (map (λ _ (measure thk2)) (range SAMPLES))))
  (if (and (= (length (remove-duplicates thk1-vals)) 1)
           (= (length (remove-duplicates thk2-vals)) 1))
      (not (equal? (first thk1-vals) (first thk2-vals)))
      (< (student-t-test thk1-vals thk2-vals) LEVEL)))

(define (measure thk)
  (define-values (_results cpu _real _gc)
    (time-apply thk '()))
  cpu)

(define (clamp-to-iqr vals)
  (define q1 (quantile 0.25 < vals))
  (define q3 (quantile 0.75 < vals))
  (filter (λ (x) (<= q1 x q3)) vals))
