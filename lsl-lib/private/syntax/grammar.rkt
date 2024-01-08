#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require ee-lib/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (for-syntax contract-literal)
 Flat
 Function
 OneOf
 AllOf
 Struct
 List
 Tuple
 Recursive

 (for-syntax flat-literal)
 check
 generate
 shrink
 symbolic

 (for-syntax function-literal)
 arguments
 result
 raises)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; literal forms

(define-literal-forms contract-literal
  "contract constructor must occur within a contract"
  (Flat Function OneOf AllOf Struct List Tuple Recursive))

(define-literal-forms flat-literal
  "literal clause must occur within Flat"
  (check generate shrink symbolic))

(define-literal-forms function-literal
  "literal clause must occur within Function"
  (arguments result raises))
