#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/match
         (only-in rosette/safe
                  define-symbolic*
                  assume)
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
    (init-field syntax checker [domain #f] [generator #f] [shrinker #f])

    (super-new)

    (define/override (protect val pos)
      (if (checker val)
          (passed-guard
           (λ (val neg) val))
          (failed-guard
           (λ (val neg)
             (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (if generator (generator fuel) (none)))

    (define/override (shrink fuel val)
      (if shrinker (shrinker fuel val) (none)))

    (define/override (interact mode val)
      (none))

    (define/override (symbolic)
      (cond
        [domain
         (define-symbolic* val domain)
         (assume (checker val))
         val]
        [else (none)]))))
