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
 member? ; TODO: Fix this.
 remove-all

 (lift-out
  build-list
  range
  ; TODO: Verify definitions below behave like ISL.
  list*
  make-list
  ; TODO memq?
  memv)

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
   ^member ; TODO: Fix this.
   ^memq ; TODO: Fix this.
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

(define (remove-all x l)
  (filter (lambda (y) (not (equal? y x))) l))
