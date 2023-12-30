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
         racket/list
         racket/string
         "contract.rkt")

;;
;; function
;;

(define (function-contract stx dom-order doms cod)
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
  (define (generate)
    (define gen
      (λ/memoize args
        (contract-generate-function (apply cod args))))
    (procedure-reduce-arity gen n))
  (define (interact mode val)
    (define ((dom-apply acc k) dom)
      (mode (apply dom acc)))
    (define args (list-update-many doms dom-order dom-apply))
    (parameterize ([current-verify-arguments args])
      (apply val args))
    (void))
  (define self (contract-struct stx protect generate #false interact))
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
