#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
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
      (define val* (unproxy val))
      (define (field-guards-passed? val)
        (cond
          [(predicate val)
           (define guards
             (for/list ([ctc (in-list contracts)]
                        [field (in-list (struct->list val))])
               (send ctc protect field pos)))
           (andmap passed-guard? guards)]
          [else #f]))
      (if (field-guards-passed? val*)
          (passed-guard
           (λ (val neg)
             (proxy val this)))
          (failed-guard
           (λ (val neg)
             (flat-error this syntax predicate val* pos)))))

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
          (apply constructor (map replace-none fields fields*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Struct accessor and mutator must use contract field of proxy.

* Interact.

* Symbolic.

|#
