#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/string
                     racket/provide-transform
                     syntax/parse
                     syntax/stx)
         (only-in rosette/safe
                  for/all)
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax strip)
         (struct-out none)
         λ/memoize
         repeat/fuel
         repeat/fix
         lift-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct none ())

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

(define (repeat/fix f x)
  (define x* (f x))
  (if (equal? x x*) x (repeat/fix f x*)))

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
         (for/all ([x x #:exhaustive])
           (go (cons x acc) xt))]))))
