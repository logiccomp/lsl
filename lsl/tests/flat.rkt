#lang racket/base

(require chk
         "util.rkt")

;; pre-defined flat
(chk
 (run/var Integer x 10 x)  10
 (run/var Real x 3.14 x)  3.14
 (run/var Boolean x #t x)  #t
 #:? integer?
 (run (contract-generate Integer))
 #:? real?
 (run (contract-generate Real))
 #:? boolean?
 (run (contract-generate Boolean))
 (run/var Boolean x #t (check-contract x))  (void)
 (run/var Boolean x #t (verify-contract x))  (void))

;; pre-defined flat failure
(chk
 #:x (run/var Integer x 1/2 x)
 "expected: Integer"
 #:x (run/var Real x #t x)
 "expected: Real"
 #:x (run/var Boolean x 0 x)
 "expected: Boolean")

;; user-defined flat
(chk
 #:do (define even-sexp
        '(define-contract Even
           (Flat (domain Integer)
                 (check even?)
                 (generate (λ () (* 2 (contract-generate Integer)))))))
 (run/sexp `(begin ,even-sexp (: x Even) (define x 2) x))  2
 #:x (run/sexp `(begin ,even-sexp (: x Even) (define x 1) x))
 "expected: Even"
 #:? even?
 (run/sexp `(begin ,even-sexp (contract-generate Even))))
