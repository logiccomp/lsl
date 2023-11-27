#lang info

;; general

(define name "lsl")
(define collection "lsl")
(define scribblings
  '(["scribblings/lsl.scrbl" ()]))

;; dependencies

(define deps
  '("errortrace-lib"
    "base"
    "lsl-lib"))

(define implies
  '("lsl-lib"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))