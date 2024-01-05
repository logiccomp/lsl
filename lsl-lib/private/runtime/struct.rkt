#lang racket/base

;;
;; provide
;;

(provide (struct-out root-struct)
         (struct-out proxy-struct)
         struct-contract
         unproxy-struct)

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
         racket/struct
         "contract.rkt"
         "flat.rkt")

;;
;; data
;;

(struct root-struct ()
  #:transparent
  #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc self other recur)
     (recur (unproxy-struct self) (unproxy-struct other)))
   (define (hash-proc self recur)
     (recur (unproxy-struct self)))
   (define (hash2-proc self recur)
     (recur (unproxy-struct self)))])

(struct proxy-struct root-struct (target protects)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (recur (unproxy-struct self) port))])

(define (unproxy-struct st)
  (if (proxy-struct? st)
      (unproxy-struct (proxy-struct-target st))
      st))

;;
;; syntax
;;

(define (struct-contract stx make struct-pred? . ctcs)
  (define ((protect self) val pos)
    (define !val (unproxy-struct val))
    (unless (struct-pred? !val)
      (contract-error self stx pos val))
    (for ([ctc (in-list ctcs)]
          [field (in-list (struct->list !val))])
      (((contract-struct-protect ctc) ctc) field pos))
    (λ (neg)
      (define field-protects
        (for/vector ([ctc (in-list ctcs)])
          (define p ((contract-struct-protect ctc) ctc))
          (λ (val) ((p val pos) neg))))
      (proxy-struct val field-protects)))
  (define (predicate val)
    (define !val (unproxy-struct val))
    (and (struct-pred? !val)
         (for/and ([ctc (in-list ctcs)]
                   [field (in-list (struct->list !val))])
           ((flat-contract-struct-predicate ctc) field))))
  (define (generate fuel)
    (define fields
      (for/list ([ctc (in-list ctcs)])
        (contract-generate-function ctc fuel)))
    (if (ormap contract-generate-failure? fields)
        (contract-generate-failure)
        (apply make fields)))
  (define (shrink val)
    (define fields
      (for/list ([ctc (in-list ctcs)]
                 [field (in-list (struct->list val))])
        (contract-shrink-function ctc field)))
    (apply make fields))
  (if (andmap flat-contract-struct? ctcs)
      (flat-contract-struct stx protect generate shrink #f #f predicate)
      (contract-struct stx protect generate shrink #f #f)))
