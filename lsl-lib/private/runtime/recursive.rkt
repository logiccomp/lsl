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

(define (recursive-contract thk)
  (define name '|anonymous contract|)
  (define (predicate val)
    ((flat-contract-struct-predicate (thk)) val))
  (define (protect val pos)
    (λ (neg)
      (((contract-struct-protect (thk)) val pos) neg)))
  (flat-contract-struct
   name
   protect
   #false
   #false
   #false
   predicate))
