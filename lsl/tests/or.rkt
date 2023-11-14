#lang racket/base

(require chk
         "util.rkt")

;; success
(chk
 (run/var (one-of Integer Boolean) x 10 x)  10
 (run/var (one-of Integer Boolean) x #t x)  #t)

;; failure
(chk
 #:x (run/var (one-of Integer Boolean) x 1/2 x)
 "expected: anonymous contract")
