#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require ee-lib/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (for-syntax contract-literal)
 Immediate
 Function
 OneOf
 AllOf
 Struct
 List
 Tuple
 Recursive
 All
 Exists
 Seal

 (for-syntax immediate-literal)
 check
 generate
 shrink
 feature

 (for-syntax function-literal)
 arguments
 result
 raises)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; literal forms

(define-literal-forms contract-literal
  "contract constructor must occur within a contract"
  (Immediate Function OneOf AllOf Struct List Tuple Recursive All Exists Seal))

(define-literal-forms immediate-literal
  "literal clause must occur within Immediate"
  (check generate shrink feature))

(define-literal-forms function-literal
  "literal clause must occur within Function"
  (arguments result raises))
