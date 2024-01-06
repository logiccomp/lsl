#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out none)
         λ/memoize
         repeat/fuel
         repeat/fix)

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
