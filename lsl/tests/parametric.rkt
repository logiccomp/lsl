#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: id (All (A) (-> A A)))
        (define (id x) x)
        (list (id 1) (id "foo")))
   '(1 "foo")
   #:x
   (run (: id (All (A) (-> A A)))
        (define (id x) 1)
        (id "foo"))
   "expected: ∀A₁"

   (run (: counter-pkg (Exists (A) (Tuple (-> A) (-> A A) (-> A Integer))))
        (define counter-pkg
          (list (λ () 0)
                (λ (x) (+ x 1))
                (λ (x) x)))
        (define make-counter (first counter-pkg))
        (define counter-incr (second counter-pkg))
        (define counter-get (third counter-pkg))
        (counter-get (counter-incr (counter-incr (make-counter)))))
   2
   #:x
   (run (: make-counter (Exists (A) (-> A)))
        (define (make-counter) 0)
        (+ 1 (make-counter)))
   "expected real?"
   ))
