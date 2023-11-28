#lang racket/base

(require chk
         racket/sandbox
         racket/string
         "util.rkt")

(chk
 #:do
 (define output
   (call-with-trusted-sandbox-configuration
    (λ ()
      (define str (open-output-string))
      (parameterize ([sandbox-error-output str])
        (make-module-evaluator
         '(module m lsl/lang
            (check-expect 1 1)
            (check-expect 1 2))))
      (get-output-string str))))
 #:t (string-contains? output "1 success(es) 1 failure(s) 0 error(s) 2 test(s) run"))
