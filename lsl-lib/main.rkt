#lang racket/base

;;
;; provide
;;

(provide (all-from-out "no-gui.rkt")
         (all-from-out automata/machine)
         (all-from-out automata/dfa)
         (all-from-out automata/re)
         (all-from-out automata/re-ext)
         visualize
         ticks)

;;
;; require
;;

(require (only-in automata/machine
                  [machine-accepting? accepting?]
                  [machine-accepts? accepts?])
         (only-in automata/dfa
                  [dfa state-machine])
         (only-in automata/re
                  [re regular-expression]
                  complement
                  seq
                  union
                  star
                  epsilon)
         (only-in automata/re-ext
                  [seq/close seq-prefix])
         racket/lazy-require
         "no-gui.rkt")

(lazy-require
 ["gui.rkt" (visualize ticks)])

;;
;; reader
;;

(module reader syntax/module-reader
  #:language 'lsl
  #:info
  (λ (key default use-default)
    (case key
      [(drracket:opt-out-toolbar-buttons) #f]
      [else (use-default key default)])))
