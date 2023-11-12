#lang info

;; general

(define name "lsl")
(define collection "lsl")
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))

;; dependencies

(define deps
  '("https://github.com/michaelballantyne/syntax-spec.git"
    "base"
    "errortrace-lib"
    "mischief"
    "rosette"))

(define build-deps '())
