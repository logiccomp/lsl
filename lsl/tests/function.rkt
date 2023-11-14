#lang racket/base

(require chk
         "util.rkt")

;; success
(chk
 (run/var (-> Integer Integer) f (λ (x) x) (f 10))  10
 (run/var (function [x Integer] (flat (check (λ (y) (eq? x y)))))
          f
          (λ (x) x)
          (f 10))
 10
 #:? integer?
 (run ((contract-generate (-> Integer Integer)) 10))
 (run/var (-> Integer Integer) f (λ (x) x) (contract-exercise f))  (void)
 (run/var (-> Integer Integer) f (λ (x) x) (contract-verify f))  (void))

;; failure
(chk
 #:x (run/var (-> Integer Boolean) f (λ (x) x) (f 10))
 "expected: Boolean"
 #:x (run/var (-> Integer Boolean) f (λ (x) x) (contract-exercise f))
 "expected: Boolean"
 #:x (run/var (function [x Integer] (flat (check (λ (y) (eq? x y)))))
              f
              (λ (x) (+ x 1))
              (f 10))
 "expected: anonymous contract"
 #:x (run (define-contract Even
            (flat
             (domain Integer)
             (check even?)))
          (: f (-> Even Even))
          (define (f x) (+ x 1))
          (contract-verify f))
 "expected: Even")

;; fizzbuzz
(chk
 #:do (define fb
        '(define-contract FizzBuzz
           (flat
            (domain Integer)
            (check (lambda (x) (not (or (zero? (modulo x 3)) (zero? (modulo x 5))))))
            (generate (λ () (+ (* 15 (contract-generate Integer)) 1))))))

 (run/sexp `(begin ,fb
                   (: x FizzBuzz)
                   (define x 4)
                   x))
 4

 (run/sexp `(begin ,fb
                   (: f (-> Integer FizzBuzz))
                   (define (f x) (+ (* 15 x) 1))
                   (contract-verify f)))
 (void)

 #:x (run/sexp `(begin ,fb
                       (: x FizzBuzz)
                       (define x 3)
                       x))
 "expected: FizzBuzz"

 #:x (run/sexp `(begin ,fb
                       (: f (-> Integer FizzBuzz))
                       (define (f x) x)
                       (contract-verify f)))
 "expected: FizzBuzz")
