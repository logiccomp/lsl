#lang racket/base

;;
;; provide
;;

(provide function-contract)

;;
;; require
;;

(require racket/list
         "contract.rkt")

;;
;; function
;;

(define (function-contract stx dom-order doms cod)
  (define n (length doms))
  (define (protect val pos)
    (unless (procedure? val)
      (contract-error pos stx val))
    (unless (procedure-arity-includes? val n)
      (contract-error pos stx val))
    (λ (neg)
      (define (wrapper . args)
        (define ((dom-apply acc k) arg)
          (define dom (list-ref doms k))
          (define proj (contract-struct-protect (apply dom acc)))
          ((proj arg pos) neg))
        (define args* (list-update-many args dom-order dom-apply))
        (define (results-wrapper res)
          (define proj (contract-struct-protect (apply cod args*)))
          ((proj res pos) neg))
        (apply values (cons results-wrapper args*)))
      (chaperone-procedure
       val wrapper
       impersonator-prop:contract self)))
  (define (generated . args)
    ((contract-struct-generate (apply cod args))))
  (define (interact mode val)
    (define ((dom-apply acc k) dom)
      (mode (apply dom acc)))
    (apply val (list-update-many doms dom-order dom-apply))
    (void))
  (define self
    (contract-struct
     stx
     protect
     (λ () (procedure-reduce-arity generated n))
     #false
     interact))
  self)

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))
