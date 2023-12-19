#lang racket/base

;;
;; provide
;;

(provide (all-from-out "gui.rkt")
         (all-from-out "no-gui.rkt"))

;;
;; require
;;

(require "gui.rkt"
         "no-gui.rkt")

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
