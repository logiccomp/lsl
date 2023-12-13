#lang racket/base

;;
;; provide
;;

(provide struct-contract)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         (only-in rosette/safe
                  solvable?
                  symbolic?
                  assert
                  assume
                  verify
                  sat?
                  evaluate
                  complete-solution)
         racket/bool
         racket/struct
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define (struct-contract stx make struct-pred? . ctcs)
  (define ((protect self) val pos)
    (unless (struct-pred? val)
      (contract-error self pos stx val))
    (λ (neg)
      (define fields (struct->list val))
      (define new-fields
        (for/list ([ctc (in-list ctcs)]
                   [field (in-list fields)])
          ((((contract-struct-protect ctc) ctc) field pos) neg)))
      (apply make new-fields)))
  (define (predicate val)
    (and (struct-pred? val)
         (for/and ([ctc (in-list ctcs)]
                   [field (in-list (struct->list val))])
           ((flat-contract-struct-predicate ctc) field))))
  (if (andmap flat-contract-struct? ctcs)
      (flat-contract-struct (syntax->datum stx) stx protect #f #f #f predicate)
      (contract-struct (syntax->datum stx) stx protect #f #f #f)))
