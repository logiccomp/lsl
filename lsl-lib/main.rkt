#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
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

(lazy-require
 ["private/library/concurrency-gui.rkt" (start-gui)])

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
 start-gui

 (except-out
  (all-from-out "private/syntax/grammar.rkt")
  Seal)

 (all-from-out
  "private/library/automata.rkt"
  "private/library/boolean.rkt"
  "private/library/contract.rkt"
  "private/library/core.rkt"
  "private/library/equal.rkt"
  "private/library/function.rkt"
  "private/library/list.rkt"
  "private/library/number.rkt"
  "private/library/string.rkt"
  "private/library/char.rkt"
  "private/library/test.rkt")

 (except-out
  (all-from-out "private/library/concurrency.rkt")
  action-state
  action-packets)

 (all-from-out "private/library/time.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  #:language 'lsl
  #:info
  (λ (key default use-default)
    (case key
      [(drracket:opt-out-toolbar-buttons) #f]
      [else (use-default key default)])))
