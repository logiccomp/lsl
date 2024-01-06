#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require ee-lib/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (for-syntax contract-literal)
 Flat

 (for-syntax flat-literal)
 domain
 check
 generate
 shrink

 (for-syntax function-literal)
 arguments
 result
 raises

 (for-syntax contract-syntax-rep?
             contract-syntax-transform)
 define-contract-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; literal forms

(define-literal-forms contract-literal
  "contract constructor must occur within a contract"
  (Flat))

(define-literal-forms flat-literal
  "literal clause must occur within Flat"
  (domain check generate shrink))

(define-literal-forms function-literal
  "literal clause must occur within Function"
  (arguments result raises))

(define-extensible-syntax contract-syntax)
