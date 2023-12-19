#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require plot
         racket/list
         syntax/parse/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualize

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
                 [plot-y-label "Time (Ticks)"]
                 [plot-brush-color-map 'tab10]
                 [plot-pen-color-map 'tab10])
    (plot pts)))

(define (ticks thk)
  (define before (current-memory-use 'cumulative))
  (thk)
  (define after (current-memory-use 'cumulative))
  (- after before))
