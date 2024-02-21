#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in ^ rosette/safe)
         (only-in rosette/base/core/exn
                  exn:fail:svm:assume?)
         racket/bool
         racket/class
         racket/format
         racket/list
         racket/set
         racket/string
         "../guard.rkt"
         "../proxy.rkt"
         "../util.rkt"
         "common.rkt")

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
      (skip-symbolic
       val
       (define val-proc? (procedure? val))
       (define val-arity? (and val-proc? (procedure-arity-includes? val arity)))
       (if val-arity?
           (passed-guard
            (λ (val neg)
              (define (info . args)
                (define n-args (length args))
                (unless (= n-args arity)
                  (contract-error this syntax val pos
                                  #:expected (args-error arity)
                                  #:given (args-error n-args)))
                (define ((dom-apply acc k) arg)
                  (define make-dom (list-ref domains k))
                  (define guard (send (apply make-dom acc) protect arg neg))
                  (guard arg pos))
                (define args* (list-update-many args domain-order dom-apply))
                (define cur (current-allowed-exns))
                (define exn-set (and cur (set-union cur exceptions)))
                (define result
                  (parameterize ([current-allowed-exns exn-set])
                    (apply val args*)))
                (define guard (send (apply codomain args*) protect result pos))
                (guard result neg))
              (proc val info this (λ () val))))
           (failed-guard
            (λ (val neg)
              (if val-proc?
                  (contract-error this syntax val pos
                                  #:expected (format ARITY-FMT arity)
                                  #:given (format ARITY-FMT (procedure-arity val)))
                  (contract-error this syntax val pos)))))))

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
      (cond
        [(ormap none? args) #f]
        [else
         (define pos (positive-blame #f #f))
         (define neg (negative-blame #f #f))
         (define soln
           (^verify
            (parameterize ([current-allowed-exns null]
                           [current-disable-contract (eq? mode symbolic-mode)])
              (define result (apply val args))
              (parameterize ([current-disable-contract #f])
                (define guard (send (apply codomain args) protect result pos))
                (guard result neg)))))
         (cond
           [(^sat? soln)
            (define concrete-args
              (^evaluate args (^complete-solution soln (^symbolics args))))
            (define-values (best-args best-exn)
              (find-best-args val concrete-args (fail-exn val concrete-args)))
            (list (if (empty? best-args)
                      (format "(~a)" name)
                      (format "(~a ~a)" name (string-join (map ~v best-args))))
                  best-exn)]
           [else #f])]))

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
      (define res
        (^with-vc
         (parameterize ([current-allowed-exns null])
           (apply val args))))
      (define exn (^result-value res))
      (and (^failed? res)
           (not (exn:fail:svm:assume? exn))
           exn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants and helpers

(define SHRINK-FUEL 10)
(define ARITY-FMT "~a-arity function")

(define (args-error n)
  (if (= n 1)
      (format "~a argument" n)
      (format "~a arguments" n)))

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))
