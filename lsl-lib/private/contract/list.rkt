#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/list
         racket/class
         "../guard.rkt"
         "../util.rkt"
         "common.rkt")

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
        (cond
          [(not (list? val)) #f]
          [(not fixed?) (map (λ (elem) (send elems-ctc protect elem pos)) val)]
          [(not (= (length val) list-size)) #f]
          [else (map (λ (ctc elem) (send ctc protect elem pos)) contracts val)]))
      (define guard-ctor
        (if (and guards (andmap passed-guard? guards))
            passed-guard
            failed-guard))
      (guard-ctor
       (λ (val neg)
         (unless guards
           (contract-error this syntax val pos))
         (map (λ (guard elem) (guard elem neg)) guards val))))

    (define/override (generate fuel)
      (if fixed?
          (for/list ([ctc (in-list contracts)])
            (send ctc generate fuel))
          (build-list
           (random (add1 fuel))
           (λ _ (send elems-ctc generate fuel)))))

    (define/override (shrink fuel val)
      (cond
        [(or fixed? (empty? val)) (shrink-elems fuel val)]
        [(< (random) 1/2) (shrink-length fuel val)]
        [else (shrink-elems fuel val)]))

    (define (shrink-length fuel val)
      (define k (random (length val)))
      (append (take val k) (drop val (add1 k))))

    (define (shrink-elems fuel val)
      (for/list ([ctc (in-cycle (in-list contracts))]
                 [elem (in-list val)])
        (send ctc shrink fuel elem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Interact.


|#
