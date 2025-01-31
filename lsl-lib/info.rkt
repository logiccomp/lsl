#lang info

;; general

(define name "lsl")
(define collection "lsl")
(define pkg-desc "Logical Student Language")
(define version "0.0")
(define pkg-authors '(camoy dbp))

;; dependencies

(define deps
  '("gui-lib"
    "t-test"
    "gui-easy-lib"
    "metapict"
    "automata-lib"
    "http-easy-lib"
    "plot-gui-lib"
    "plot-lib"
    "rackunit-lib"
    "struct-set"
    "threading-lib"
    "base"
    "ee-lib"
    "errortrace-lib"
    "mischief"
    "syntax-classes"
    "threading"))

(define build-deps '())
