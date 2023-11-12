#lang racket/base

(provide run)

(define-syntax-rule (run e ...)
  (run/lsl '(begin e ...)))

(define (run/lsl sexp)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'lsl/lang))
  (eval sexp ns))
