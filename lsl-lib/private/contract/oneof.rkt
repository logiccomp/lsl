#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in racket/base
                  for/list
                  for/fold
                  for*/first
                  in-list
                  in-value
                  raise-user-error)
         racket/random
         racket/class
         racket/list
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide oneof-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define oneof-contract%
  (class contract%
    (init-field syntax disjuncts)

    (super-new)

    (define/override (protect val pos)
      (define gvs (filter passed-guard? (guards-of val pos)))
      (if (= (length gvs) 1)
          (first gvs)
          (failed-guard
           (λ (val neg)
             (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (define disjunct (random-ref disjuncts))
      (send disjunct generate fuel))

    (define/override (shrink fuel val)
      (for*/first ([disjunct (in-list (shuffle disjuncts))]
                   [guard (in-value (send disjunct protect val #f))]
                   #:when (not (failed-guard? guard)))
        (send disjunct shrink fuel val)))

    (define (guards-of val pos)
      (map (λ (disjunct) (send disjunct protect val pos))
           disjuncts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Generate for `oneof` with no disjuncts should be equivalent
  to generate for `None`.

* Interact.

|#
