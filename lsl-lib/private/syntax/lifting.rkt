#lang racket/base

(provide lift-out)

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse
                     syntax/stx)
         (only-in rosette/safe for/all)
         racket/match)

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
