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
  (define (generated . args)
    (define ((cod-apply acc k) cod)
      (define both (append args acc))
      ((contract-struct-generate (apply cod both))))
    (apply values (list-update-many cods cod-order cod-apply)))
  (define (interact mode val)
    (define ((dom-apply acc k) dom)
      (mode (apply dom acc)))
    (apply val (list-update-many doms dom-order dom-apply))
    (void))
  (define self
    (contract-struct
     'function
     protect
     (λ () (procedure-reduce-arity generated n))
     #false
     interact))
  self)

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))
