#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/base
         racket/provide
         "../util.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (lift-out
  char->integer
  char-alphabetic?
  char-ci<=?
  char-ci<?
  char-ci=?
  char-ci>=?
  char-ci>?
  char-downcase
  char-lower-case?
  char-numeric?
  char-upcase
  char-upper-case?
  char-whitespace?
  char<=?
  char<?
  char=?
  char>=?
  char>?
  char?))