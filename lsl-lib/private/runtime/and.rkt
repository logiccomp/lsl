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
         mischief/stream
         racket/bool
         racket/match
         racket/list
         racket/stream
         racket/random
         "contract.rkt"
         "flat.rkt")

;;
;; syntax
;;

(define (and-contract stx . ctcs)
  (define preds (map flat-contract-struct-predicate ctcs))
  (define (predicate val)
    (for/and ([pred (in-list preds)])
      (pred val)))
  (define (generate fuel)
    (match-define (cons fst-ctc rst-ctc) ctcs)
    (let go ([k (* 10 fuel)])
      (cond
        [(zero? k) (contract-generate-failure)]
        [else
         (define v (contract-generate-function fst-ctc fuel))
         (define v-good?
           (and (not (contract-generate-failure? v))
                (for/and ([pred (in-list (rest preds))])
                  (pred v))))
         (if v-good? v (go (sub1 k)))])))
  (define (shrink val)
    (define smaller-streams
      (for/list ([ctc (in-list ctcs)])
        (contract-shrink-function ctc val)))
    (stream-filter predicate (apply stream-interleave smaller-streams)))
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
