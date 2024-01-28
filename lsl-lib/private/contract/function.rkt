#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/format
         racket/list
         racket/string
         (prefix-in ^ rosette/safe)
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
      (define val-proc? (procedure? val))
      (define val-arity? (and val-proc? (procedure-arity-includes? val arity)))
      (if val-arity?
          (passed-guard
           (λ (val neg)
             (define (info . args)
               (define ((dom-apply acc k) arg)
                 (define make-dom (list-ref domains k))
                 (define guard (send (apply make-dom acc) protect arg neg))
                 (guard arg pos))
               (define args* (list-update-many args domain-order dom-apply))
               (define result (apply val args*))
               (define guard (send (apply codomain args*) protect result pos))
               (guard result neg))
             (proc val info this)))
          (failed-guard
           (λ (val neg)
             (if val-proc?
                 (contract-error this syntax val pos
                                 #:expected (format ARITY-FMT arity)
                                 #:given (format ARITY-FMT (procedure-arity val)))
                 (contract-error this syntax val pos))))))

    (define/override (generate fuel)
      (define generated
        (λ/memoize args
          (let ([result (send (apply codomain args) generate fuel)])
            (when (none? result)
              (generate-error syntax))
            result)))
      (procedure-reduce-arity generated arity))

    (define/override (interact val name mode)
      (define ((dom-apply acc k) dom)
        (mode (apply dom acc)))
      (define args (list-update-many domains domain-order dom-apply))
      (define (does-fail failed-exn)
        (define concrete-args
          (if (ormap ^symbolic? args)
              (^result-value
               (^with-vc (exn:root-vc failed-exn)
                         (^evaluate args (^solve (void)))))
              args))
        (define-values (best-args best-exn)
          (find-best-args val concrete-args failed-exn))
        (list (if (empty? best-args)
                  (format "(~a)" name)
                  (format "(~a ~a)" name (string-join (map ~v best-args))))
              best-exn))
      (cond
        [(ormap none? args) (none)]
        [(fail-exn val args) => does-fail]
        [else #f]))

    (define (find-best-args val args last-exn)
      (define args* (shrink* args))
      (cond
        [(or (ormap none? args*) (equal? args args*))
         (values args last-exn)]
        [(fail-exn val args*) => (λ (exn) (find-best-args val args* exn))]
        [else
         (values args last-exn)]))

    (define (shrink* args)
      (define ((shrink-apply acc k) dom)
        (send (apply dom acc) shrink SHRINK-FUEL (list-ref args k)))
      (list-update-many domains domain-order shrink-apply))

    (define (fail-exn val args)
      (define (exn:user-handler exn)
        (define val (exn:user-value exn))
        (define allowed?
          (for/or ([exn-pred? (in-list exceptions)])
            (exn-pred? val)))
        (if allowed? #f exn))
      (with-handlers ([exn:contract? values]
                      [exn:user? exn:user-handler])
        (apply val args)
        #f))))

(define SHRINK-FUEL 10)

(define ARITY-FMT "~a-arity function")

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))
