#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/string
                     syntax/id-table
                     syntax/id-set
                     syntax/parse
                     threading)
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax strip
                     kebab->camel
                     camel->kebab
                     contract-table-ref
                     contract-table-set!
                     mutable-struct-set)
         (struct-out base-seal)
         (struct-out exn:fail:gave-up)
         (struct-out exn:fail:invalid)
         give-up
         current-logs
         current-pbt-stats
         λ/memoize
         any?
         any-list?
         error-if-parametric
         current-allowed-exns
         current-traces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct base-seal ())

(define current-logs (make-parameter #f))
(define current-pbt-stats (make-parameter #f))
(define current-allowed-exns (make-parameter #f))
(define current-traces (make-parameter (hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exns

(struct exn:fail:gave-up exn:fail:syntax ())
(struct exn:fail:invalid exn:fail:syntax (witness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memoize

(define-syntax λ/memoize
  (syntax-parser
    [(_ args:id body:expr)
     #'(let ([table (make-hash)])
         (λ args (hash-ref! table args (λ () body))))]))

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
  (define mutable-struct-set (mutable-free-id-set))

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

(define (give-up stx)
  (raise
   (exn:fail:gave-up
    "contract-generate: failed to generate value satisfying contract"
    (current-continuation-marks)
    (list (syntax-property stx 'unexpanded)))))
