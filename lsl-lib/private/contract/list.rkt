#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/bool
         racket/class
         racket/list
         racket/struct
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide list-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define list-contract%
  (class contract%
    (init-field syntax fixed? contracts)

    (super-new)

    (define elems-ctc (and (not fixed?) (first contracts)))
    (define list-size (and fixed? (length contracts)))

    (define/override (protect val pos)
      (define guards
        (and (list? val)
             (implies fixed? (= (length val) list-size))
             (for/list ([ctc (if fixed?
                                 (in-list contracts)
                                 (in-cycle (in-value elems-ctc)))]
                        [elem (in-list val)])
               (send ctc protect elem pos))))
      (define guard-ctor
        (if (and guards (andmap passed-guard? guards))
            passed-guard
            failed-guard))
      (guard-ctor
       (λ (val neg)
         (unless guards
           (contract-error this syntax val pos))
         (for/list ([guard (in-list guards)]
                    [elem (in-list val)])
           (guard elem neg)))))

    (define/override (generate fuel)
      (define result
        (if fixed?
            (for/list ([ctc (in-list contracts)])
              (send ctc generate fuel))
            (build-list
             (random (add1 fuel))
             (λ _ (send elems-ctc generate fuel)))))
      (if (ormap none? result)
          (none)
          result))

    (define/override (shrink fuel val)
      (if (or fixed? (empty? val))
          (shrink-elems fuel val)
          (try (list shrink-length shrink-elems) fuel val)))

    (define (try fs fuel val)
      (for/fold ([acc (none)])
                ([f (in-list fs)])
        #:break (not (none? acc))
        (f fuel val)))

    (define (shrink-length fuel val)
      (define k (random (length val)))
      (append (take val k) (drop val (add1 k))))

    (define (shrink-elems fuel val)
      (define result
        (for/list ([ctc (in-cycle (in-list contracts))]
                   [elem (in-list val)])
          (send ctc shrink fuel elem)))
      (if (ormap none? result) (none) result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Interact.

* Symbolic.

|#
