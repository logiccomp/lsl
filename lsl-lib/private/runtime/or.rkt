#lang racket/base

;;
;; provide
;;

(provide or-contract)

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
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define (or-contract stx ctcs)
  (define preds (map flat-contract-struct-predicate ctcs))
  (define (predicate val)
    (for/or ([pred (in-list preds)])
      (pred val)))
  (define protect (flat-contract-protect stx predicate))
  (flat-contract-struct stx protect #f #f #f predicate))
