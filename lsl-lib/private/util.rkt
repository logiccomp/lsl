#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/string
                     syntax/id-table
                     syntax/parse
                     threading)
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax strip
                     kebab->camel
                     camel->kebab
                     contract-table-ref
                     contract-table-set!)
         (struct-out none)
         (struct-out base-seal)
         current-logs
         λ/memoize
         repeat/fuel
         any?
         any-list?
         error-if-parametric
         current-allowed-exns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct none ())
(struct base-seal ())

(define current-logs (make-parameter #f))
(define current-allowed-exns (make-parameter #f))

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
  (define (kebab->camel stx)
    (~> stx
        syntax-e
        symbol->string
        (string-replace "-" " ")
        string-titlecase
        (string-replace " " "")
        string->symbol
        (datum->syntax stx _)))

  (define (camel->kebab stx)
    (~> stx
        syntax-e
        symbol->string
        (regexp-replace* #rx"([a-z])([A-Z])" _ "\\1-\\2")
        string-downcase
        string->symbol
        (datum->syntax stx _)))

  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

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
     (and (list? xs) (andmap any? xs)))))

(define (error-if-parametric x)
  (when (base-seal? x)
    (error 'if "cannot use parametric value ~a" x))
  x)
