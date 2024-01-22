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

; TODO: Add the following definitions (should be easy to define with Rosette? )
; boolean->string
; false?

(provide
 (contract-out
  [rename equal? boolean=? (-> ^boolean? ^boolean? ^boolean?)])

 (filtered-out
  (strip "^")
  (combine-out
   ^boolean?
   ^false
   ^not
   ^true)))
