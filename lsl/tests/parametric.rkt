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

   #:t
   (run* (: id (All (A) (-> A A)))
         (define (id x) x)
         (check-contract id))

   #:x
   (run (: id (All (A) (-> A A)))
        (define (id x) 1)
        (id "foo"))
   "expected: ∀A"

   #:x
   (run* (: id (All (A) (-> A A)))
         (define (id x) 1)
         (check-contract id))
   "expected: ∀A"

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

   (run (: pkg (Exists (A) (Tuple (-> A Integer) A)))
        (define pkg (list identity 3))
        (: unpkg (-> (Exists (A) (Tuple (-> A Integer) A)) Integer))
        (define (unpkg p)
          ((first p) (second p)))
        (list ((first pkg) (second pkg))
              (unpkg pkg)))
   '(3 3)

   #:x
   (run (: make-counter (Exists (A) (-> A)))
        (define (make-counter) 0)
        (+ 1 (make-counter)))
   "expected real?"
   ))
