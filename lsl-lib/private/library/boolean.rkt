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
  [rename equal? boolean=? (-> is-boolean? is-boolean? is-boolean?)]
  [rename ^boolean? boolean? (-> any? is-boolean?)]
  [rename ^false? false? (-> any? is-boolean?)]
  [rename ^not not (-> is-boolean? is-boolean?)])

 (filtered-out
  (strip "^")
  (combine-out
   ^false
   ^true)))
