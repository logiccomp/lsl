#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/contract
         racket/provide
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [rename equal? boolean=? (-> ^boolean? ^boolean? ^boolean?)]
  [rename ^boolean? boolean? (-> any? ^boolean?)]
  [rename ^not not (-> any? ^boolean?)])

 (filtered-out
  (strip "^")
  (combine-out
   ^false
   ^true)))
