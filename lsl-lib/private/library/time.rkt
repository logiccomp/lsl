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
  (define all-vals
    (for/list ([k (in-range (* SAMPLES 2))])
      (if (even? k)
          (measure thk1)
          (measure thk2))))
  (define thk1-vals
    (for/list ([val (in-list all-vals)]
               [k (in-naturals)]
               #:when (even? k))
      val))
  (define thk2-vals
    (for/list ([val (in-list all-vals)]
               [k (in-naturals)]
               #:when (odd? k))
      val))
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
