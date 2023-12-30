#lang racket/base

;;
;; provide
;;

(provide and-contract)

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
         racket/match
         racket/list
         racket/random
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define ATTEMPTS 100)

(define (and-contract stx . ctcs)
  (define preds (map flat-contract-struct-predicate ctcs))
  (define (predicate val)
    (for/and ([pred (in-list preds)])
      (pred val)))
  (define (generate)
    (match-define (cons fst-ctc rst-ctc) ctcs)
    (define gen (contract-generate-function fst-ctc #t))
    (let go ([k ATTEMPTS])
      (cond
        [(zero? k) (error "generator failed")]
        [else
         (define v (gen))
         (define v-satisfies?
           (for/and ([pred (in-list (rest preds))])
             (pred v)))
         (if v-satisfies? v (go (sub1 k)))])))
  (define protect (flat-contract-protect stx predicate))
  (define self
    (flat-contract-struct
     stx
     protect
     generate
     #f
     #f
     predicate))
  self)
