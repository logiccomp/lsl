#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in plot ticks)
         racket/list
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
      (define (shorten lst)
        (let ((l (length lst)))
          (cond
            [(> l 3) (take lst 3)]
            [(> l 1) (take lst 1)]
            [else lst])))
      (define _warmup
        (for/list ([arg (in-list (shorten args))])
          (measure (λ () (f arg)))))
      (define results
        (for/list ([arg (in-list args)])
          (measure (λ () (f arg)))))
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
