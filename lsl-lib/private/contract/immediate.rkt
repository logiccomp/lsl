#lang rosette/safe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide immediate-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define immediate-contract%
  (class contract%
    (init-field syntax checker [generator #f] [shrinker #f] [symbol #f])

    (super-new)

    (define/override (protect val pos)
      (skip-symbolic
       val
       (if (checker val)
           (passed-guard
            (λ (val neg) val))
           (failed-guard
            (λ (val neg)
              (contract-error this syntax val pos))))))

    (define/override (generate fuel)
      (if generator (generator fuel) (none)))

    (define/override (shrink fuel val)
      (if shrinker (shrinker fuel val) (none)))

    (define/override (interact val name mode)
      #f)

    (define/override (symbolic)
      (if symbol (symbol) (none)))))
