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
         "private/library/contract.rkt"
         "private/library/core.rkt"
         "private/library/equal.rkt"
         "private/library/function.rkt"
         "private/library/list.rkt"
         "private/library/number.rkt"
         "private/library/string.rkt"
         "private/library/test.rkt")

(lazy-require
 ["private/library/performance.rkt" (ticks visualize)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 define-contract
 (rename-out
  [define-protected define]
  [declare-contract :])

 (all-from-out
  "private/syntax/grammar.rkt"
  "private/library/automata.rkt"
  "private/library/boolean.rkt"
  "private/library/contract.rkt"
  "private/library/core.rkt"
  "private/library/equal.rkt"
  "private/library/function.rkt"
  "private/library/list.rkt"
  "private/library/number.rkt"
  "private/library/string.rkt"
  "private/library/test.rkt")

 ticks
 visualize)

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
