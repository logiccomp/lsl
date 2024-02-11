#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/string
                     racket/provide-transform
                     syntax/id-table
                     syntax/parse
                     syntax/stx)
         (prefix-in ^ rosette/safe)
         racket/contract
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax strip
                     contract-table-ref
                     contract-table-set!)
         (struct-out none)
         (struct-out base-seal)
         λ/memoize
         repeat/fuel
         lift-out
         any?
         any-list?
         error-if-parametric)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct none ())
(struct base-seal ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memoize

(define-syntax λ/memoize
  (syntax-parser
    [(_ args:id body:expr)
     #'(let ([table (make-hash)])
         (λ args (hash-ref! table args (λ () body))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repeat

(define (repeat/fuel f fuel)
  (cond
    [(zero? fuel) (none)]
    [else
     (define x (f))
     (if (none? x) (repeat/fuel f (sub1 fuel)) x)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(begin-for-syntax
  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

(define-syntax lift-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ f ...)
        #:with (wrapped-f ...)
        (stx-map
         syntax-local-lift-expression
         #'((wrap-lift f) ...))
        (pre-expand-export
         #'(rename-out [wrapped-f f] ...)
         modes)]))))

(define (wrap-lift f)
  (λ args
    (let go ([acc null] [args args])
      (match args
        [(list) (apply f (reverse acc))]
        [(cons x xt)
         (^for/all ([x x #:exhaustive])
           (go (cons x acc) xt))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract table

(begin-for-syntax
  (define free-contract-table (make-free-id-table))

  (define (contract-table-set! id val)
    (free-id-table-set! free-contract-table id val)
    (void))

  (define (contract-table-ref id)
    (free-id-table-ref free-contract-table id #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define any?
  (flat-named-contract
   'non-parametric?
   (not/c base-seal?)))

(define any-list?
  (flat-named-contract
   'list-without-parametric?
   (λ (xs)
     (and (^list? xs)
          (^andmap any? xs)))))

(define (error-if-parametric x)
  (when (base-seal? x)
    (error 'if "cannot use parametric value ~a" x))
  x)
