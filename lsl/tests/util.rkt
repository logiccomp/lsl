#lang racket/base

(provide run
         run/sexp
         run/var)

(define-syntax-rule (run/var ctc var val body ...)
  (run (: var ctc)
       (define var val)
       body ...))

(define-syntax-rule (run e ...)
  (run/sexp '(begin e ...)))

(define (run/sexp sexp)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'lsl/lang))
  (eval sexp ns))
