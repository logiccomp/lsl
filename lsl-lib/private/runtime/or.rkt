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

(define (or-contract maybe-name ctcs)
  (define name (or maybe-name '|anonymous contract|))
  (define preds (map flat-contract-struct-predicate ctcs))
  (define (predicate val)
    (for/or ([pred (in-list preds)])
      (pred val)))
  (define protect (flat-contract-protect name predicate))
  (flat-contract-struct name protect #f #f #f predicate))
