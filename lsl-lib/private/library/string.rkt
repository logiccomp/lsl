#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/provide
         racket/string
         racket/list
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (explode s)
  (drop-right (drop (string-split s "") 1) 1))

(define (implode l)
  (apply string-append l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (lift-out
  explode
  implode
  format
  list->string
  make-string
  string
  string->list
  string->number
  number->string
  string->symbol
  string-append
  string-contains?
  string-copy
  string-downcase
  string-length
  string-ref
  string-upcase
  string=?
  string?
  substring
  string<?
  ; TODO: Verify all definitions below behave like ISL.
  string-ci<=?
  string-ci<?
  string-ci=?
  string-ci>=?
  string-ci>?
  string<=?
  string>=?
  string>?
  ; TODO: int->string
  ; TODO: replicate
  ; TODO: string->int
  ; TODO: string-alphabetic?
  ; TODO: string-contains-ci?
  ; TODO: string-ith
  ; TODO: string-lower-case?
  ; TODO: string-numeric?
  ; TODO: string-upper-case?
  ; TODO: string-whitespace?
  ))
