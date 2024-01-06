#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/list
         racket/unsafe/ops
         "common.rkt"
         "../guard.rkt"
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide function-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define function-contract%
  (class contract%
    (init-field syntax domain-order domains codomain exceptions)

    (super-new)

    (define arity (length domains))

    (define/override (protect val pos)
      (define (procedure-valid? val)
        (and (procedure? val)
             (procedure-arity-includes? val arity)))
      (if (procedure-valid? val)
          (passed-guard
           (λ (val neg)
             (define (wrapper . args)
               (define ((dom-apply acc k) arg)
                 (define make-dom (list-ref domains k))
                 (define guard (send (apply make-dom acc) protect arg pos))
                 (guard arg neg))
               (define args* (list-update-many args domain-order dom-apply))
               (define result (apply val args*))
               (define guard (send (apply codomain args*) protect result pos))
               (guard result neg))
             ;; TODO: Should be `(proxy val this)` with contract checks
             ;; moved into elimination form (same as for structs)
             (proxy (unsafe-impersonate-procedure val wrapper) this)))
          (failed-guard
           (λ (val neg)
             (contract-error this syntax val pos)))))

    (define/override (generate fuel)
      (define generated
        (λ/memoize args
          (let ([result (send (apply codomain args) generate fuel)])
            (when (none? result)
              (generate-error syntax))
            result)))
      (procedure-reduce-arity generated arity))

    (define/override (interact mode val)
      (define ((dom-apply acc k) dom)
        (mode (apply dom acc)))
      (define args (list-update-many domains domain-order dom-apply))
      (if (fails? args)
          (repeat/fix shrink* args)
          (none)))

    (define (fails? val args)
      (define (contract-exn-handler _) #t)
      (define (user-exn-handler exn)
        (define val (exn:fail:user-value exn))
        (for/and ([exn-pred? (in-list exceptions)])
          (not (exn-pred? val))))
      (with-handlers ([exn:fail:contract? contract-exn-handler]
                      [exn:fail:user? user-exn-handler])
        (apply val args)
        #f))

    (define (shrink* args)
      (define ((shrink-apply acc k) dom)
        (send (apply dom acc) shrink (list-ref args k)))
      (list-update-many domains domain-order shrink-apply))))

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Restore arity error.

* Generated value should be protected to ensure clients can't misuse it.

|#
