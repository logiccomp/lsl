#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/match
         (prefix-in ^ rosette/safe)
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide parametric-contract%
         seal-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(struct seal-info (ctor pred? get polarity))

(define parametric-contract%
  (class contract%
    (init-field syntax polarity names make-body)

    (super-new)

    (define/override (protect val pos)
      (define infos
        (for/list ([name (in-list names)])
          (struct seal (val))
          (seal-info seal seal? seal-val polarity)))
      (define ctc (apply make-body infos))
      (send ctc protect val pos))))

(define seal-contract%
  (class contract%
    (init-field syntax info)

    (super-new)

    (define/override (protect val pos)
      (match-define (seal-info ctor pred? get polarity) info)
      (define wrap?
        (or (and polarity (negative-blame? pos))
            (and (not polarity) (positive-blame? pos))))
      (cond
        [wrap? (passed-guard (λ (val neg) (ctor val)))]
        [(pred? val) (passed-guard (λ (val neg) (get val)))]
        [else
         (failed-guard
          (λ (val neg)
            (contract-error this syntax val pos)))]))))
