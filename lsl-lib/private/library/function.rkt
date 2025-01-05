#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/contract
         racket/list
         racket/function
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [andmap (-> (unconstrained-domain-> boolean?) list? list? ... any)]
  [ormap (-> (unconstrained-domain-> boolean?) list? list? ... any)]
  [procedure? (-> any? boolean?)])

 apply
 argmax
 argmin
 compose
 filter
 foldl
 foldr
 for-each
 identity
 map
 memf
 sort)
