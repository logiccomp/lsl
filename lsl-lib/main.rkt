#lang racket/base

;;
;; provide
;;

(provide (all-from-out "no-gui.rkt")
         visualize)

;;
;; require
;;

(require racket/lazy-require
         "no-gui.rkt")

(lazy-require
 ["gui.rkt" (visualize)])

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
