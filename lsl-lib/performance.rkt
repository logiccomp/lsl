#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in plot ticks)
         racket/list
         syntax/parse/define
         "private/library/time.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define SYMS '(fullcircle fullsquare fulldiamond full5star))
(define POINT-SIZE 8)

(define (visualize args . fs)
  (define pts
    (for/list ([f (in-list fs)]
               [k (in-naturals)]
               [sym (in-cycle SYMS)])
      (define name (or (object-name f) '???))
      (define results
        (for/list ([arg (in-list args)])
          (ticks (λ () (f arg)))))
      (points (map vector (range (length args)) results)
              #:color k
              #:fill-color k
              #:sym sym
              #:size POINT-SIZE
              #:label (symbol->string name))))
  (parameterize ([plot-x-label "Argument (Index)"]
                 [plot-y-label "Time (Milliseconds)"]
                 [plot-brush-color-map 'tab10]
                 [plot-pen-color-map 'tab10])
    (plot pts)))
