#lang racket/base

(require chk
         racket/contract
         "util.rkt")

;; success
(chk
 (run/var (OneOf Integer Boolean) x 10 x)  10
 (run/var (OneOf Integer Boolean) x #t x)  #t
 #:? (or/c 0 1)
 (run (contract-generate (OneOf (Constant 0) (Constant 1)))))

;; failure
(chk
 #:x (run/var (OneOf Integer Boolean) x 1/2 x)
 "expected: (OneOf Integer Boolean)")
