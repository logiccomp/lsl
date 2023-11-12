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

(define (flat-contract maybe-name dom check generate symbolic)
  (define name (or maybe-name '|anonymous contract|))
  (define dom-pred (and dom (flat-contract-struct-predicate dom)))
  (define dom-name (and dom (contract-struct-name dom)))
  (define (predicate x)
    (and (implies dom-pred (dom-pred x))
         (implies check (check x))))
  (define protect (flat-contract-protect name predicate))
  (define symbolic*
    (or symbolic
        (and dom
             (λ ()
               (define x ((contract-struct-symbolic dom)))
               (assume (implies check (check x)))
               x))))
  (define (interact mode val) (void))
  (flat-contract-struct name protect generate symbolic* interact predicate))

(define (flat-contract-protect name predicate)
  (λ (val pos)
    (when (symbolic? val)
      (define result (verify (assert (predicate val))))
      (when (sat? result)
        (define counter-eg (evaluate val (complete-solution result (list val))))
        (verify-error pos name counter-eg)))
    (when (concrete? val)
       (unless (predicate val)
         (contract-error pos name val)))
    (λ (neg) val)))
