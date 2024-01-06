#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/promise
         "common.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide recursive-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define recursive-contract%
  (class contract%
    (init-field syntax promise)

    (super-new)

    (define/override (protect val pos)
      (send (force promise) protect val pos))

    (define/override (generate fuel)
      ;; TODO: reduce fuel here?
      (send (force promise) generate fuel))

    (define/override (shrink fuel val)
      (send (force promise) shrink fuel val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Macroify.

* Interact.

* Symbolic.

|#
