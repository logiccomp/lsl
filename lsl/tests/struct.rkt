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

 #:t
 (run (define-struct foo (x))
      (: f (-> Any (Foo Integer)))
      (define (f x) x)
      (define orig (make-foo 10))
      (define prox (f orig))
      (set-foo-x! prox 17)
      (and (= (foo-x orig) (foo-x prox) 17)
           (equal? orig prox)
           (equal? prox orig)
           (equal? (format "~a" prox)
                   (format "~a" orig))))

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
      (: f (-> Any (Foo Integer)))
      (define (f x) x)
      (set-foo-x! (f (make-foo 10)) ""))
 "expected: Integer"

 #:x
 (run (define-struct foo (x))
      (: f (-> Any (Foo Integer)))
      (define (f x) x)
      (define orig (make-foo 10))
      (define prox (f orig))
      (set-foo-x! orig "")
      (foo-x prox))
 "expected: Integer"

 #:x
 (run (define-struct foo (x))
      (: f (-> (Foo Integer) Integer))
      (define (f st) (foo-x st))
      (f (make-foo 1/2)))
 "expected: Integer")
