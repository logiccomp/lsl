#lang racket/base

(provide run
         run*
         run/sexp
         syntax/unexpanded)

(require racket/port
         racket/match)

(define-syntax-rule (run e ...)
  (run/sexp 'e ...))

(define-syntax-rule (run* e ...)
  (run/sexp 'e ... #:no-result #t))

(define ns (make-base-namespace))

(define (run/sexp #:no-result [no-result #f] . sexps)
  (match-define (list a ... b) sexps)
  (define mod-name (gensym))
  (define prog
    (if no-result
        `(module ,mod-name lsl ,@sexps)
        `(module ,mod-name lsl
           ,@a
           (define result ,b)
           (provide result))))
  (define result #t)
  (define output
    (call-with-output-string
     (λ (p)
       (parameterize ([current-output-port (open-output-string)]
                      [current-error-port p]
                      [current-namespace ns])
         (eval prog)
         (if no-result
             (dynamic-require `(submod (quote ,mod-name) main) #f)
             (set! result (dynamic-require `(quote ,mod-name) 'result)))))))
  (if (equal? output "")
      result
      (error output)))

(define-syntax-rule (syntax/unexpanded tmpl)
  (syntax-property #'tmpl 'unexpanded #'tmpl))
