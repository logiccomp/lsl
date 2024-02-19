#lang rosette/safe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in racket/base
                  for/list
                  for/fold
                  for*/first
                  in-list
                  in-value
                  raise-user-error)
         racket/class
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
      (skip-symbolic
       val
       (define gvs (filter passed-guard? (guards-of val pos)))
       (if (= (length gvs) 1)
           (first gvs)
           (failed-guard
            (λ (val neg)
              (contract-error this syntax val pos))))))

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
      (map (λ (disjunct) (send disjunct protect val pos))
           disjuncts))

    (define/override (symbolic)
      (cond
        [(empty? disjuncts)
         (raise-user-error 'symbolic "cannot create symbolic value for empty disjunction")]
        [else
         (let go ([disjuncts disjuncts])
           (cond
             [(empty? (rest disjuncts)) (send (first disjuncts) symbolic)]
             [else
              (define x (first disjuncts))
              (define xt (rest disjuncts))
              (define-symbolic* b boolean?)
              (if b (send x symbolic) (go xt))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Generate for `oneof` with no disjuncts should be equivalent
  to generate for `None`.

* Interact.

|#
