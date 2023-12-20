#lang racket/base

;;
;; provide
;;

(provide oneof-contract)

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
         racket/random
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define (oneof-contract stx . ctcs)
  (define preds (map flat-contract-struct-predicate ctcs))
  (define (predicate val)
    (for/or ([pred (in-list preds)])
      (pred val)))
  (define (generate)
    ((random-ref (map contract-struct-generate ctcs))))
  (define protect (flat-contract-protect stx predicate))
  (define self
    (flat-contract-struct
     (syntax->datum stx)
     stx
     protect
     generate
     #f
     #f
     predicate))
  self)
