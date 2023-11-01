#lang racket/base

;;
;; provide
;;

(provide function-contract)

;;
;; require
;;

(require "contract.rkt")

;;
;; function
;;

(define (function-contract doms cods)
  (define (protect val pos)
    (contract-error pos procedure? val)
    (contract-error pos procedure-arity-includes? val (length doms))
    (λ (neg)
      (define (wrapper . args)
        (define args*
          (for/list ([dom (in-list doms)]
                     [arg (in-list args)])
            (define proj (contract-struct-protect (apply dom args)))
            ((proj arg pos) neg)))
        (define (results-wrapper . results)
          (define args+results (append args results))
          (define results*
            (for/list ([cod (in-list cods)]
                       [res (in-list results)])
              (define proj (contract-struct-protect (apply cod args+results)))
              ((proj res pos) neg)))
          (apply values results*))
        (apply values (cons results-wrapper args*)))
      (chaperone-procedure
       val wrapper
       impersonator-prop:contract self)))
  (define self (contract-struct protect #false))
  self)
