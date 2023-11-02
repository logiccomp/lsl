#lang racket/base

;;
;; provide
;;

(provide (for-syntax function-dependencies)
         function-contract)

;;
;; require
;;

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     syntax/parse
                     syntax/id-table
                     mischief/dict
                     mischief/sort)
         racket/list
         "contract.rkt"
         "../util.rkt")

;;
;; function
;;

(define (function-contract dom-order doms cod-order cods)
  (define n (length doms))
  (define (protect val pos)
    (unless (procedure? val)
      (contract-error pos 'procedure? val))
    (unless (procedure-arity-includes? val n)
      (contract-error pos 'procedure-arity-includes? val))
    (λ (neg)
      (define (wrapper . args)
        (define ((dom-apply acc k) arg)
          (define dom (list-ref doms k))
          (define proj (contract-struct-protect (apply dom acc)))
          ((proj arg pos) neg))
        (define args* (list-update-many args dom-order dom-apply))
        (define (results-wrapper . results)
          (define ((cod-apply acc k) res)
            (define cod (list-ref cods k))
            (define both (append args* acc))
            (define proj (contract-struct-protect (apply cod both)))
            ((proj res pos) neg))
          (define results* (list-update-many results cod-order cod-apply))
          (apply values results*))
        (apply values (cons results-wrapper args*)))
      (chaperone-procedure
       val wrapper
       impersonator-prop:contract self)))
  (define generated
    (procedure-reduce-arity
     (λ args (apply values (map contract-generate-function cods)))
     n))
  (define (interact val)
    (define args (range n))
    (define ((dom-apply acc k) dom)
      ((contract-struct-generate (apply dom acc))))
    (list-update-many doms dom-order dom-apply))
  (define self
    (contract-struct
     'function
     protect
     (λ () generated)
     interact))
  self)

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))

;;
;; dependency
;;

(begin-for-syntax
  (define (function-dependencies stxs)
    (define id-index-table
      (for/fold ([acc (make-immutable-bound-id-table)])
                ([stx (in-list stxs)]
                 [k (in-naturals)])
        (syntax-parse stx
          [(name:id body:expr)
           (bound-id-table-set acc #'name k)])))
    (define dep-hash
      (for/hash ([stx (in-list stxs)])
        (syntax-parse stx
          [(name:id body:expr)
           (define deps
             (for/list ([var (in-list (free-variables #'body))])
               (bound-id-table-ref id-index-table var #false)))
           (values #'name (filter values deps))])))
    (define neighbors (dict->procedure #:failure (const empty) dep-hash))
    (topological-sort (range (length stxs)) neighbors)))
