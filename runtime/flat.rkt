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
         racket/bool
         "contract.rkt")

;;
;; syntax
;;

(define-syntax contract
  (syntax-parser
    #:datum-literals (domain check generate)
    [(_ (~alt (~optional (domain dom-expr:expr))
              (~optional (check check-expr:expr))
              (~optional (generate gen-expr:expr)))
        ...)
     #'(flat-contract
        (~? dom-expr #false)
        (~? check-expr #false)
        (~? gen-expr #false))]))

(define (flat-contract dom check generate)
  (define (protect val pos)
    (unless (implies dom (dom val))
      (contract-error pos (object-name dom) val))
    (unless (implies check (check val))
      (contract-error pos (object-name check) val))
    (λ (neg) val))
  (define (interact val) (void))
  (contract-struct
   (object-name check)
   protect
   generate
   interact))

(define (value->flat-contract val)
  (if (contract-struct? val)
      val
      (flat-contract #false val #false)))
