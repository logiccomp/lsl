#lang racket/base

;;
;; provide
;;

(provide flat-contract)

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
  (define (protect val pos)
    (cond
      [(symbolic? val)
       (define domres (verify (assert (implies dom (dom-pred val)))))
       (when (sat? domres)
         (verify-error pos
                       name
                       (evaluate val (complete-solution domres (list val)))))
       (define checkres (verify (assert (implies check (check val)))))
       (when (sat? checkres)
         (verify-error pos
                       name
                       (evaluate val (complete-solution checkres (list val)))))]
      [else
       (unless (implies dom (dom-pred val))
         (contract-error pos name val))
       (unless (implies check (check val))
         (contract-error pos name val))])
    (λ (neg) val))
  (define symbolic*
    (or symbolic
        (and dom
             (λ ()
               (define x ((contract-struct-symbolic dom)))
               (assume (implies check (check x)))
               x))))
  (define (interact mode val) (void))
  (flat-contract-struct name protect generate symbolic* interact predicate))
