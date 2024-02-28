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
  [rename ^andmap andmap (-> (unconstrained-domain-> is-boolean?) ^list? ^list? ... any)]
  [rename ^ormap ormap (-> (unconstrained-domain-> is-boolean?) ^list? ^list? ... any)]
  [rename ^procedure? procedure? (-> any? is-boolean?)])

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
