#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in automata/machine
                  machine?
                  machine-accepting?
                  machine-accepts?)
         (only-in automata/dfa
                  dfa)
         (only-in automata/re
                  re
                  complement
                  seq
                  union
                  star
                  epsilon)
         (only-in automata/re-ext
                  [seq/close seq-prefix])
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out automata/machine)
         (all-from-out automata/dfa)
         (all-from-out automata/re)
         (all-from-out automata/re-ext)
         (contract-out
          [machine-next (-> machine? any/c machine?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (machine-next mach x)
  (mach x))
