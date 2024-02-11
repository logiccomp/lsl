#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (prefix-in ^ rosette/safe)
         racket/contract
         racket/list
         racket/provide
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 empty

 ;; TODO: Really should use `any-list?` with all list operations.
 (contract-out
  [member? (-> any? any-list? any)]
  [rename ^member member (-> any? any-list? any)]
  [rename ^memq memq (-> any? any-list? any)]
  [rename ^remove remove (-> any? any-list? any)]
  [remove-all (-> any? any-list? any)])

 (lift-out
  build-list
  range)

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
   ^null
   ^null?
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
