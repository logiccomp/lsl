#lang info

;; general

(define name "lsl")
(define collection "lsl")
(define scribblings
  '(["scribblings/lsl.scrbl" ()]))

;; dependencies

(define deps
  '("base"
    "lsl-lib"))

(define implies
  '("lsl-lib"))

(define build-deps
  '())
