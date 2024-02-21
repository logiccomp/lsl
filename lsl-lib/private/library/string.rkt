#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/contract
         racket/provide
         racket/string
         racket/list
         "time.rkt"
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
 (contract-out
  [rename $string=? string=? (-> string? string? any)])

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
  string?
  substring
  string<?))

(define ($string=? x y)
  (^for*/all ([x x #:exhaustive] [y y #:exhaustive])
    (current-ticks (+ (current-ticks) (min (string-length x) (string-length y))))
    (string=? x y)))
