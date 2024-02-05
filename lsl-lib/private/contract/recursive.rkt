#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/promise
         "common.rkt"
         "../util.rkt")

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
      (if (zero? fuel)
          (none)
          (send (force promise) generate (sub1 fuel))))

    (define/override (shrink fuel val)
      (send (force promise) shrink fuel val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

#|

* Macroify.

* Interact.

* Symbolic.

|#
