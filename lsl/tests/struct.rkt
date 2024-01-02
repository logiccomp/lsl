#lang racket/base

(require chk
         "util.rkt")

;; success
(chk
 (run (define-struct foo (x))
      (: f (-> (Foo Integer) Integer))
      (define (f st) (foo-x st))
      (f (make-foo 10)))
 10

 #:? integer?
 (run (define-struct foo (x))
      (foo-x (contract-generate (Foo Integer))))

 (run (define-struct foo (x))
      (foo-x (contract-shrink (Foo Integer) (make-foo 10))))
 5)

;; failure
(chk
 #:x
 (run (define-struct foo (x))
      (: f (-> (Foo Integer) Integer))
      (define (f st) (foo-x st))
      (f (make-foo 1/2)))
 "expected: Integer")
