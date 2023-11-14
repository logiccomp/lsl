#lang racket/base

(require chk
         "util.rkt")

;; success
(chk
 (run (define-struct foo (x))
      (: f (-> (Foo Integer) Integer))
      (define (f st) (foo-x st))
      (f (foo 10)))
 10)

;; failure
(chk
 #:x
 (run (define-struct foo (x))
      (: f (-> (Foo Integer) Integer))
      (define (f st) (foo-x st))
      (f (foo 1/2)))
 "expected: Integer")
