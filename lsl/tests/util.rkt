#lang racket/base

(provide run
         run/sexp
         syntax/unexpanded)

(require racket/port
         racket/string)

(define-syntax-rule (run e ...)
  (run/sexp '(begin e ...)))

(define (run/sexp sexp)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'lsl))
  (define result #f)
  (define output
    (call-with-output-string
     (λ (p)
       (parameterize ([current-output-port (open-output-string)]
                      [current-error-port p])
         (set! result (eval sexp ns))
         (eval '(run-tests) ns)))))
  (if (or (string-contains? output "FAILURE")
          (string-contains? output "ERROR"))
      (error output)
      result))

(define-syntax-rule (syntax/unexpanded tmpl)
  (syntax-property #'tmpl 'unexpanded #'tmpl))
