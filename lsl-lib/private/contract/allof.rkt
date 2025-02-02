#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/match
         racket/random
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide allof-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define allof-contract%
  (class contract%
    (init-field syntax conjuncts)

    (super-new)

    (define/override (protect val pos)
      (define guards (guards-of val pos))
      (define guard-ctor
        (if (andmap passed-guard? guards)
            passed-guard
            failed-guard))
      (guard-ctor
       (λ (val neg)
         (for/fold ([val val])
                   ([guard (in-list guards)])
           (guard val neg)))))

    (define/override (generate fuel)
      (define conjunct (random-ref conjuncts))
      (satisfies-immediate (send conjunct generate fuel)))

    (define/override (shrink fuel val)
      (define conjunct (random-ref conjuncts))
      (satisfies-immediate (send conjunct shrink fuel val)))

    (define (satisfies-immediate val)
      (unless (andmap passed-guard? (guards-of val #f))
        (give-up syntax))
      val)

    (define (guards-of val pos)
      (let go ([conjuncts conjuncts])
        (match conjuncts
          [(list) null]
          [(cons conjunct conjuncts-rest)
           (define guard (send conjunct protect val pos))
           (if (passed-guard? guard)
               (cons guard (go conjuncts-rest))
               (list guard))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Generate for `allof` with no conjuncts should be equivalent
  to generate for `Any`.

* Generators/shrinkers for higher-order values is tricky. In the above
  implementation, we're not guaranteed to get back something that
  satisfies higher-order constraints.

* Interact.

* Symbolic.

|#
