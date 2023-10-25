#lang info

;; general

(define collection "lsl")
(define scribblings '(("scribblings/lsl.scrbl" ())))
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))

;; dependencies

(define deps
  '("base"
    "rosette"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))
