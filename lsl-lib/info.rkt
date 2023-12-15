#lang info

;; general

(define name "lsl")
(define collection "lsl")
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))

;; dependencies

(define deps
  '("rackunit-lib"
    "struct-set"
    "threading-lib"
    "base"
    "ee-lib"
    "errortrace-lib"
    "mischief"
    "rosette"
    "syntax-classes"
    "threading"))

(define build-deps '())
