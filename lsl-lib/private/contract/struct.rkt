#lang rosette/safe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in racket/base
                  for
                  for/list
                  for/vector
                  in-list)
         racket/class
         racket/struct
         "common.rkt"
         "../guard.rkt"
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide struct-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define struct-contract%
  (class contract%
    (init-field syntax constructor predicate accessors mutators contracts)

    (super-new)

    (define/override (protect val pos)
      (skip-symbolic
       val
       (for/all ([val val])
         (define val* (unproxy val))
         (define guards
           (and (predicate val*)
                (for/list ([ctc (in-list contracts)]
                           [field (in-list (struct->list val*))])
                  (send ctc protect field pos))))
         (if (and guards (andmap passed-guard? guards))
             (passed-guard
              (λ (val neg)
                (for/all ([val val])
                  (define info
                    (for/vector ([ctc (in-list contracts)])
                      (λ (val)
                        ((send ctc protect val pos) val neg))))
                  (define (unwrap)
                    (define fields
                      (for/list ([ctc (in-list contracts)]
                                 [field (in-list (struct->list val*))])
                        ((send ctc protect field pos) field pos)))
                    (apply constructor fields))
                  (proxy val info this unwrap))))
             (failed-guard
              (λ (val neg)
                (unless guards
                  (contract-error this syntax val* pos))
                (for ([guard (in-list guards)]
                      [field (in-list (struct->list val*))])
                  (guard field neg))))))))

    (define/override (generate fuel)
      (define fields
        (for/list ([ctc (in-list contracts)])
          (send ctc generate fuel)))
      (if (ormap none? fields)
          (none)
          (apply constructor fields)))

    (define/override (shrink fuel val)
      (define fields (struct->list (unproxy val)))
      (define fields*
        (for/list ([ctc (in-list contracts)]
                   [field (in-list fields)])
          (send ctc shrink fuel field)))
      (define (replace-none x y)
        (if (none? x) y x))
      (if (andmap none? fields)
          (none)
          (apply constructor (map replace-none fields* fields))))

    (define/override (symbolic)
      (define fields
        (for/list ([ctc (in-list contracts)])
          (send ctc symbolic)))
      (apply constructor fields))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Struct accessor and mutator must use contract field of proxy.

* Interact.

|#
