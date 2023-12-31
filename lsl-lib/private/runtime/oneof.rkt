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
         racket/list
         racket/match
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
  (define (generate fuel)
    (let go ([ctcs (shuffle ctcs)])
      (match ctcs
        [(list) (contract-generate-failure)]
        [(cons ctc ctc-rest)
         (define result (contract-generate-function ctc fuel))
         (if (contract-generate-failure? result)
             (go ctc-rest)
             result)])))
  (define (shrink val)
    (for/first ([ctc (in-list ctcs)]
                [pred (in-list preds)]
                #:when (pred val))
      (contract-shrink-function ctc val)))
  (define protect (flat-contract-protect stx predicate))
  (define self
    (flat-contract-struct
     stx
     protect
     generate
     shrink
     #f
     #f
     predicate))
  self)
