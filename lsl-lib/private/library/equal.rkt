#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/contract
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (contract-out
  [equal? (-> any? any? any)]
  [rename $eq? eq? (-> any? any? any)]))

(define ($eq? x y)
  (eq? (unproxy-eq x) (unproxy-eq y)))

;; HACK: Cannot use `unproxy` for `eq?` because it checks contracts when
;; doing the unwrapping.
(define (unproxy-eq st)
  (if (proxy? st)
      (unproxy-eq (proxy-target st))
      st))
