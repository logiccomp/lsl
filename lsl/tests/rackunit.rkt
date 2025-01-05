#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/sandbox
         racket/string
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(define (contains-all-strings? str xs)
  (for/and ([x (in-list xs)])
    (string-contains? str x)))

(module+ test
  (chk
   #:do
   (define output
     (call-with-trusted-sandbox-configuration
      (λ ()
        (define str (open-output-string))
        (parameterize ([sandbox-run-submodules '(main)]
                       [sandbox-error-output str])
          (make-module-evaluator
           '(module m lsl
              (test-suite
               "foo"
               (check-expect (f 1) 2)
               (check-expect (f 1) 2))

              (check-expect 1 2)
              (check-expect 1 1)

              (test-suite
               "bar"
               (check-expect (f 1) 2)
               (check-expect (f 1) 1))

              (: f (-> Integer Integer))
              (define (f x) x))))
        (get-output-string str))))
   #:t
   (contains-all-strings?
    output
    '("definition-area tests > foo"
      "definition-area tests > bar"
      "definition-area tests > anonymous tests"
      "2 success(es) 4 failure(s) 0 error(s) 6 test(s) run"))

   #:t (run* (check-expect 1 1))

   #:x (run* (with-tests (check-expect 1 2)))
   "interaction-area tests"

   #:x (run* (test-suite "foo") (test-suite "foo"))
   "test suite foo already exists"

   #:x (run* (list (check-expect 1 1)))
   "cannot be inside a definition or expression"

   #:x (run* (check-expect (f #t) #t)
             (check-expect (g '()) '())
             (define-struct foo (bar))
             (define (g l) (filter (lambda (x) #t) l))
             (define (f x) (foo-bar x)))
   "1 success(es) 0 failure(s) 1 error(s) 2 test(s) run"
   ))
