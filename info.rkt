#lang info

;; general

(define collection "lsl")
(define scribblings '(("scribblings/lsl.scrbl" ())))
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))

;; dependencies

(define deps
  '("https://github.com/michaelballantyne/syntax-spec/"
    "base"
    "mischief"
    "rosette"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))
