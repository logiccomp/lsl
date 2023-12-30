#lang racket/base

;;
;; provide
;;

(provide recursive-contract)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         racket/struct
         "contract.rkt")

;;
;; syntax
;;

(define (recursive-contract stx thk)
  (define (predicate val)
    ((flat-contract-struct-predicate (thk self)) val))
  (define ((protect self) val pos)
    (λ (neg)
      (define ctc (thk self))
      ((((contract-struct-protect ctc) ctc) val pos) neg)))
  (define (generate)
    (contract-generate-function (thk self)))
  (define self
    (flat-contract-struct
     stx
     protect
     generate
     #false
     #false
     predicate))
  self)
