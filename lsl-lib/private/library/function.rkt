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
  [rename ^procedure? procedure? (-> any? ^boolean?)])

 (filtered-out
  (strip "^")
  (combine-out
   ^andmap
   ^apply
   ^argmax
   ^argmin
   ^compose
   ^filter
   ^foldl
   ^foldr
   ^for-each
   ^identity
   ^map
   ^memf
   ^ormap
   ^sort)))
