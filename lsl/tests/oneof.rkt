#lang racket/base

(require chk
         racket/contract
         "util.rkt")

;; success
(chk
 (run/var (OneOf Integer Boolean) x 10 x)  10
 (run/var (OneOf Integer Boolean) x #t x)  #t
 (run (contract-shrink (OneOf (Constant -10) Integer) 10))  5
 (run (contract-shrink (List Integer) '(0) 20))  '()
 #:? (or/c 0 1)
 (run (contract-generate (OneOf (Constant 0) (Constant 1)))))

;; failure
(chk
 #:x (run/var (OneOf Integer Boolean) x 1/2 x)
 "expected: (OneOf Integer Boolean)")
