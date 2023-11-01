#lang racket/base

;;
;; provide
;;

(provide contract
         flat-contract
         value->flat-contract)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         "contract.rkt")

;;
;; syntax
;;

(define-syntax contract
  (syntax-parser
    #:literals (check generate)
    [(_ (~alt (~once (check check-expr:expr))
              (~optional (generate gen-expr:expr)))
        ...)
     #'(flat-contract
        check-expr
        (~? gen-expr #false))]))

(define (flat-contract check generate)
  (define (protect val pos)
    (contract-error pos check val)
    (λ (neg) val))
  (define generate*
    (or generate (generate-error (object-name check))))
  (contract-struct protect generate*))

(define (value->flat-contract val)
  (if (contract-struct? val) val (flat-contract val #false)))
