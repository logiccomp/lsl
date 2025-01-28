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

(define (slow-string=? s1 s2)
  (begin (sleep 0.0001)
         (string=? s1 s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [rename slow-string=? string=? (-> string? string? any)])

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
