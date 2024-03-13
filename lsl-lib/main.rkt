#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in ^ rosette/safe)
         (prefix-in ^ rosette/solver/smt/z3)
         racket/lazy-require
         racket/provide
         "private/syntax/grammar.rkt"
         "private/syntax/interface.rkt"
         "private/library/automata.rkt"
         "private/library/boolean.rkt"
         "private/library/concurrency.rkt"
         "private/library/contract.rkt"
         "private/library/core.rkt"
         "private/library/equal.rkt"
         "private/library/function.rkt"
         "private/library/list.rkt"
         "private/library/number.rkt"
         "private/library/string.rkt"
         "private/library/char.rkt"
         "private/library/test.rkt"
         "private/library/time.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 define-contract
 define-package
 define-interface
 provide
 (rename-out
  [define-protected define]
  [declare-contract :])
 contract-generate
 contract-shrink
 contract-symbolic

 (except-out
  (all-from-out "private/syntax/grammar.rkt")
  Seal)

 (all-from-out
  "private/library/automata.rkt"
  "private/library/boolean.rkt"
  "private/library/concurrency.rkt"
  "private/library/contract.rkt"
  "private/library/core.rkt"
  "private/library/equal.rkt"
  "private/library/function.rkt"
  "private/library/list.rkt"
  "private/library/number.rkt"
  "private/library/string.rkt"
  "private/library/char.rkt"
  "private/library/test.rkt")

 (all-from-out "private/library/time.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; z3

(^current-solver (^z3 #:path (find-executable-path "z3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  #:language 'lsl
  #:info
  (λ (key default use-default)
    (case key
      [(drracket:opt-out-toolbar-buttons) #f]
      [else (use-default key default)])))
