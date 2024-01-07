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

(provide flat-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define flat-contract%
  (class contract%
    (init-field syntax checker [generator #f] [shrinker #f] [symbol #f])

    (super-new)

    (define/override (protect val pos)
      (define verified?
        (and (^symbolic? val)
             (^unsat? (^verify (^assert (checker val))))))
      (^if (or verified? (checker val))
           (passed-guard
            (λ (val neg) val))
           (failed-guard
            (λ (val neg)
              (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (if generator (generator fuel) (none)))

    (define/override (shrink fuel val)
      (if shrinker (shrinker fuel val) (none)))

    (define/override (interact val name mode)
      #f)

    (define/override (symbolic)
      (if symbol (symbol) (none)))))
