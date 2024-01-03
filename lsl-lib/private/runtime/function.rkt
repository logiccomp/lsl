#lang racket/base

;;
;; provide
;;

(provide function-contract)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         (only-in racket/contract or/c)
         racket/function
         racket/list
         racket/string
         "contract.rkt")

;;
;; function
;;

(define MAX-ATTEMPTS 10)

(define (function-contract stx dom-order doms cod exns)
  (define n (length doms))
  (define ((protect self) val pos)
    (unless (procedure? val)
      (contract-error self stx pos val))
    (unless (procedure-arity-includes? val n)
      (arity-error self stx pos val n))
    (λ (neg)
      (define (wrapper . args)
        (define ((dom-apply acc k) arg)
          (define dom (list-ref doms k))
          (define ctc (apply dom acc))
          (define proj (contract-struct-protect ctc))
          (((proj ctc) arg pos) neg))
        (define args* (list-update-many args dom-order dom-apply))
        (define (results-wrapper res)
          (define ctc (apply cod args*))
          (define proj (contract-struct-protect ctc))
          (((proj ctc) res pos) neg))
        (apply values (cons results-wrapper args*)))
      (chaperone-procedure
       val wrapper
       impersonator-prop:contract self)))
  (define (generate fuel)
    (define gen
      (λ/memoize args
        (let ([result (contract-generate-function (apply cod args) fuel)])
          (when (contract-generate-failure? result)
            (generate-error stx))
          result)))
    (procedure-reduce-arity gen n))
  (define (interact mode val [shrink? #t])
    (define ((dom-apply acc k) dom)
      (mode (apply dom acc)))
    (define init-args (list-update-many doms dom-order dom-apply))
    (define (shrink-args args)
      (define ((shrink-apply acc k) dom)
        (contract-shrink-function (apply dom acc) (list-ref args k)))
      (list-update-many doms dom-order shrink-apply))
    (define (fails? args)
      (parameterize ([current-verify-arguments args])
        (with-handlers ([exn:fail:contract? (λ _ #t)]
                        [exn:fail:user?
                         (λ (e) (not ((apply or/c exns) (exn:fail:user-value e))))])
          (apply val args)
          #f)))
    (cond
      [(fails? init-args)
       (define best-args
         (if shrink?
             (let go ([args init-args])
               (define args*
                 (for*/first ([fuel (in-range MAX-ATTEMPTS)]
                              [shrink-args (in-value (shrink-args args))]
                              #:when (not (equal? shrink-args args))
                              #:when (fails? shrink-args))
                   shrink-args))
               (if args* (go args*) args))
             init-args))
       (parameterize ([current-verify-arguments best-args])
         (with-handlers ([(λ (e)
                            (and (exn:fail:user? e)
                                 ((apply or/c exns) (exn:fail:user-value e))))
                          (λ _ (void))])
           (apply val best-args)))]
      [else (void)]))
  (define self (contract-struct stx protect generate #f #f interact))
  self)

(define-syntax λ/memoize
  (syntax-parser
    [(_ args:id body:expr)
     #'(let ([table (make-hash)])
         (λ args (hash-ref! table args (λ () body))))]))

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))

(define ARITY-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a-arity function"
     "given: ~a-arity function"
     "blaming: ~a")
   "\n  "))

(define (arity-error self stx blm val n)
  (define error-msg
    (format ARITY-FMT
            (blame-struct-name blm)
            n (procedure-arity val)
            (blame-struct-path blm)))
  (custom-error self stx error-msg))
