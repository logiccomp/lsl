#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/list
         racket/provide
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 empty
 member?

 (lift-out
  build-list)

 (filtered-out
  (strip "^")
  (combine-out
   ^append
   ^assoc
   ^assq
   ^car
   ^cdr
   ^cons
   ^cons?
   ^eighth
   ^empty?
   ^fifth
   ^first
   ^fourth
   ^length
   ^list
   ^list-ref
   ^list?
   ^memq
   ^null
   ^null?
   ^remove
   ^rest
   ^reverse
   ^second
   ^seventh
   ^sixth
   ^third)))

(define (member? x l)
  (^not (^equal? #f (^member x l))))
