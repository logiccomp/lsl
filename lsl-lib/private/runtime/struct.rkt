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
         "contract.rkt")

;;
;; syntax
;;

(define (struct-contract maybe-name make ctcs)
  (define name (or maybe-name '|anonymous contract|))
  (define (protect val pos)
    (λ (neg)
      (define fields (struct->list val))
      (define new-fields
        (for/list ([ctc (in-list ctcs)]
                   [field (in-list fields)])
          (((contract-struct-protect ctc) field pos) neg)))
      (apply make new-fields)))
  (contract-struct
   name
   protect
   #false
   #false
   #false))
