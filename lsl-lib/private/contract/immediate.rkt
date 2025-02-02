#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/match
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
    (init-field syntax checker [feature #f] [generator #f] [shrinker #f])

    (super-new)

    (define/override (protect val pos)
      (if (checker val)
          (passed-guard
           (λ (val neg) val))
          (failed-guard
           (λ (val neg)
             (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (cond
        [generator
         (define val (generator fuel))
         (unless (checker val)
           (raise
            (exn:fail:invalid
             (format "contract-generate: generated value ~a does not satisfy contract" val)
             (current-continuation-marks)
             (list (syntax-property syntax 'unexpanded))
             val)))
         val]
        [else (give-up syntax)]))

    (define/override (shrink fuel val)
      (if shrinker (shrinker fuel val) val))

    (define/override (interact val name mode)
      #f)

    (define/override (describe val)
      (for/list ([feat (in-list feature)])
        (match-define (list name func) feat)
        (cons (string->symbol name) (func val))))))
