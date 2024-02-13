#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   #:t (run (: f (-> (AllOf) Any)) (define (f x) 10) #t)
   #:t (run (define-struct foo (x))
            (: f (-> (Foo Integer) (Foo Integer)))
            (define (f x) x)
            (and (equal? (make-foo 10) (make-foo 10))
                 (equal? (f (make-foo 10)) (f (make-foo 10)))
                 (equal? (f (f (make-foo 10))) (make-foo 10))
                 (equal? (make-foo 10) (f (f (make-foo 10))))
                 (not (equal? (make-foo 10) (make-foo 11)))
                 (not (equal? (f (make-foo 10)) (f (make-foo 11))))
                 (not (equal? (f (f (make-foo 10))) (make-foo 11)))
                 (not (equal? (make-foo 10) (f (f (make-foo 11)))))))
   #:x (run* (define-struct foo (x))
             (check-expect (make-foo 10) (make-foo 11))
             #t)
   "FAILURE"
   #:x (run (define-struct foo (x))
            (define-struct bar (x y))
            (: f (-> (Foo Integer) Any))
            (define (f x) x)
            (bar-y (f (make-foo 10))))
   "expected: bar?"
   ))
