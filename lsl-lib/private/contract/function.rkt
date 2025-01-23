#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/format
         racket/function
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
                 (contract-error this syntax val pos))))))

    (define/override (generate fuel)
      (define generated
        (λ/memoize args
          (let ([result (send (apply codomain args) generate fuel)])
            (when (none? result)
              (generate-error syntax))
            result)))
      (procedure-reduce-arity generated arity))

    (define/override (interact val name contract->value)
      (define ((dom-apply acc k) dom)
        (define ctc (apply dom acc))
        (define val (contract->value ctc))
        (list val (send ctc describe val)))
      (define args+feats (list-update-many domains domain-order dom-apply))
      (define args (map first args+feats))
      (define feats (append-map second args+feats))
      (cond
        ;; Failed generation
        [(ormap none? args)
         (none)]
        [else
         (define init-exn (fail-exn val args))
         (define args-fmt
           (if (empty? args)
               (format "(~a)" name)
               (format "(~a ~a)" name (string-join (map ~v args)))))
         (cond
           ;; Found counterexample
           [init-exn
            (define-values (best-args best-exn)
              (find-best-args val args init-exn))
            (list (if (empty? best-args)
                      (format "(~a)" name)
                      (format "(~a ~a)" name (string-join (map ~v best-args))))
                  args-fmt
                  (make-immutable-hash feats)
                  best-exn)]
           ;; Good test
           [else
            (list args-fmt
                  (make-immutable-hash feats))])]))

    (define (find-best-args val args last-exn)
      (define args* (shrink* args))
      (cond
        [(or (ormap none? args*) (equal? args args*))
         (values args last-exn)]
        [(fail-exn val args*) => (λ (exn) (find-best-args val args* exn))]
        [else (values args last-exn)]))

    (define (shrink* args)
      (define ((shrink-apply acc k) dom)
        (send (apply dom acc) shrink SHRINK-FUEL (list-ref args k)))
      (list-update-many domains domain-order shrink-apply))

    (define (fail-exn val args)
      (define x
        (with-handlers ([exn? values])
          (apply val args)))
      (cond
        [(exn:fail:lsl:user? x)
         (define val (exn:fail:lsl:user-val x))
         (and (not (ormap (λ (pred?) (pred? val)) exceptions)) x)]
        [else
         (and (exn? x) x)]))))

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
