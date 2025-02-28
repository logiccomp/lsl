#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/contract
         racket/list
         "../util.rkt"
         "equal.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 empty

 ;; TODO: Really should use `any-list?` with all list operations.
 (contract-out
  [member? (-> any? any-list? any)]
  [memq? (-> any? any-list? boolean?)]
  [memq (-> any? any-list? boolean?)]
  [remove (-> any? any-list? any)]
  [list? (-> any? boolean?)]
  [cons? (-> any? boolean?)]
  [empty? (-> any? boolean?)]
  [null? (-> any? boolean?)]
  [remove-all (-> any? any-list? any)]
  [remove-duplicates (-> any-list? any-list?)])

 build-list
 range
 append
 assoc
 assq
 car
 cdr
 cons
 eighth
 fifth
 first
 fourth
 length
 list
 list-ref
 null
 rest
 reverse
 second
 seventh
 sixth
 third)

(define (member? x l)
  (not (equal? #f (member x l))))

(define (remove-all x l)
  (filter (lambda (y) (not (equal? y x))) l))

(define (memq? x l)
  (ormap (lambda (el) (eq? x el)) l))

(define memq memq?)
