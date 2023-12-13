#lang racket/base

;;
;; provide
;;

(provide flat-contract
         flat-contract-protect)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         (only-in rosette/safe
                  concrete?
                  solvable?
                  symbolic?
                  assert
                  assume
                  verify
                  sat?
                  evaluate
                  complete-solution)
         racket/bool
         "contract.rkt")

;;
;; syntax
;;

(define (flat-contract stx dom check generate symbolic)
  (define dom-pred (and dom (flat-contract-struct-predicate dom)))
  (define dom-name (and dom (contract-struct-name dom)))
  (define (predicate x)
    (and (implies dom-pred (dom-pred x))
         (implies check (check x))))
  (define protect (flat-contract-protect stx predicate))
  (define (default-symbolic)
    (define x ((contract-struct-symbolic dom)))
    (assume (implies check (check x)))
    x)
  (define symbolic* (or symbolic (and dom default-symbolic)))
  (define (interact mode val) (void))
  (define self (flat-contract-struct (syntax->datum stx) stx protect generate symbolic* interact predicate))
  self)

(define (flat-contract-protect stx predicate)
  (λ (self)
    (λ (val pos)
      (when (symbolic? val)
        (define result (verify (assert (predicate val))))
        (when (sat? result)
          (define counter-eg (evaluate val (complete-solution result (list val))))
          (verify-error self pos stx counter-eg)))
      (when (concrete? val)
        (unless (predicate val)
          (contract-error self pos stx val)))
      (λ (neg) val))))
