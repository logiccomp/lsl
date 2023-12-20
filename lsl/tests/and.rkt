#lang racket/base

(require chk
         racket/contract
         "util.rkt")

;; success
(chk
 (run/var (And Integer (Flat (check positive?))) x 10 x)  10
 #:? (and/c integer? positive?)
 (run (contract-generate (And Integer (Flat (check positive?))))))

;; failure
(chk
 #:x (run/var (And Integer (Flat (check positive?))) x -1 x)
 "expected: (And Integer (Flat (check positive?)))"
 #:x (run/var (And Integer (Flat (check positive?))) x #t x)
 "expected: (And Integer (Flat (check positive?)))")
