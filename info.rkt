#lang info

;; general

(define collection "lsl")
(define scribblings '(("scribblings/lsl.scrbl" ())))
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))
(define compile-omit-paths '("manifest.scm"))
(define test-omit-paths '("manifest.scm"))

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
