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
         (only-in rosette/safe
                  define-symbolic*
                  solvable?
                  symbolic?
                  assert
                  solve
                  unsat?)
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
  (define name (object-name check))
  (define (protect val pos)
    (unless (implies dom (dom val))
      (contract-error pos (object-name dom) val))
    (unless (implies check (check val))
      (contract-error pos (object-name check) val))
    (when (symbolic? val)
      (assert (implies dom (dom val)))
      (when (unsat? (solve #true))
        (verify-error pos (object-name dom) val))
      (assert (implies check (check val)))
      (when (unsat? (solve #true))
        (verify-error pos (object-name check) val)))
    (λ (neg) val))
  (define symbolic
    (and (solvable? dom)
         (λ () (define-symbolic* x dom) x)))
  (define (interact mode val) (void))
  (contract-struct name protect generate symbolic interact))

(define (value->flat-contract val)
  (if (contract-struct? val)
      val
      (flat-contract #false val #false)))
