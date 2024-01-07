#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/list
         racket/match
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
      (define (exactly-one? val)
        (match (filter passed-guard? (guards-of val pos))
          [(list guard) guard]
          [_ #f]))
      (or (exactly-one? val)
          (failed-guard
           (λ (val neg)
             (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (for/fold ([val (none)])
                ([disjunct (in-list (shuffle disjuncts))])
        #:break (not (none? val))
        (send disjunct generate fuel)))

    (define/override (shrink fuel val)
      (for*/first ([disjunct (in-list (shuffle disjuncts))]
                   [guard (in-value (send disjunct protect val #f))]
                   #:when (not (failed-guard? guard)))
        (send disjunct shrink fuel val)))

    (define (guards-of val pos)
      (for/list ([disjunct (in-list disjuncts)])
        (send disjunct protect val pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Generate for `oneof` with no disjuncts should be equivalent
  to generate for `None`.

* Interact.

* Symbolic.

|#
