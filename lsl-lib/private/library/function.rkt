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
  [rename ^andmap andmap (-> procedure? any-list? any-list? ... any)]
  [rename ^ormap ormap (-> procedure? any-list? any-list? ... any)]
  [rename ^procedure? procedure? (-> any? ^boolean?)])

 (filtered-out
  (strip "^")
  (combine-out
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
   ^sort)))
