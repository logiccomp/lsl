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
         mischief/stream
         racket/bool
         racket/stream
         racket/struct
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define (struct-contract stx make struct-pred? . ctcs)
  (define ((protect self) val pos)
    (unless (struct-pred? val)
      (contract-error self stx pos val))
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
  (define (generate fuel)
    (define fields
      (for/list ([ctc (in-list ctcs)])
        (contract-generate-function ctc fuel)))
    (if (ormap contract-generate-failure? fields)
        (contract-generate-failure)
        (apply make fields)))
  (define (shrink val)
    (define field-streams
      (for/list ([ctc (in-list ctcs)]
                 [field (in-list (struct->list val))])
        (contract-shrink-function ctc field)))
    (stream-map (λ (fields) (apply make fields))
                (stream-zip field-streams)))
  (if (andmap flat-contract-struct? ctcs)
      (flat-contract-struct stx protect generate shrink #f #f predicate)
      (contract-struct stx protect generate shrink #f #f)))
