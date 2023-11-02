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
    (unless (procedure? val)
      (contract-error pos 'procedure? val))
    (unless (procedure-arity-includes? val (length doms))
      (contract-error pos 'procedure-arity-includes? val))
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
  (define generated
    (procedure-reduce-arity
     (λ args (apply values (map contract-generate-function cods)))
     (length doms)))
  ;; TODO: doesn't work because doms are functions
  (define (interact val)
    (apply val (map contract-generate-function doms)))
  (define self
    (contract-struct
     'function
     protect
     (λ () generated)
     interact))
  self)
