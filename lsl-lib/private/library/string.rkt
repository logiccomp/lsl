#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/contract
         racket/string
         racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (explode s)
  (drop-right (drop (string-split s "") 1) 1))

(define (implode l)
  (apply string-append l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [string=? (-> string? string? any)])

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
 string?
 substring
 string<?)
