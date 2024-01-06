#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette integer? even?)
         chk
         racket/class
         lsl/private/contract/flat
         lsl/private/guard
         lsl/private/util
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (define bool-ctc
    (new flat-contract%
         [syntax #'Boolean]
         [checker boolean?]
         [domain boolean?]
         [generator (λ (fuel) (< (random) 1/2))]
         [shrinker (λ (fuel val) #f)]))

  (define pos-ctc
    (new flat-contract%
         [syntax #'Positive]
         [checker (λ (x) (and (real? x) (positive? x)))]
         [domain real?]
         [generator (λ (fuel) (random fuel))]
         [shrinker (λ (fuel val) (floor (/ val 2)))]))

  (define even-ctc
    (new flat-contract%
         [syntax #'Even]
         [checker (λ (x) (and (integer? x) (even? x)))]
         [domain integer?]
         [generator (λ (fuel) (* 2 (random fuel)))]
         [shrinker (λ (fuel val)
                     (define val* (floor (/ val 2)))
                     (if (odd? val*) (sub1 val*) val*))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send even-ctc protect 2 #f)
   ((send even-ctc protect 2 #f) 2 #f)  2

   #:? failed-guard?
   (send even-ctc protect 3 #f)
   #:x ((send even-ctc protect 3 #f) 3 #f)
   "expected: Even"

   #:? even?
   (send even-ctc generate 1)
   (send even-ctc shrink 1 4)  2
   (send even-ctc shrink 1 5)  2

   #:? none?
   (send even-ctc interact 'generate 1)

   #:? even?
   (send even-ctc symbolic)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

#|
;; pre-defined flat
(chk
 (run/var Integer x 10 x)  10
 (run/var Real x 3.14 x)  3.14
 (run/var Boolean x #t x)  #t
 (run/var (List Boolean) x (list #t #f) x) (list #t #f)
 (run/var (List 2 Boolean) x (list #t #f) x) (list #t #f)
 #:? (λ (xs) (andmap integer? xs))
 (run (contract-generate (List Integer)))
 #:? integer?
 (run (contract-generate Integer))
 #:? real?
 (run (contract-generate Real))
 #:? boolean?
 (run (contract-generate Boolean))
 (run/var Boolean x #t (check-contract x))  (void)
 (run/var Boolean x #t (verify-contract x))  (void))

;; record
(chk
 #:do (define no-fold-sexp
        '(begin
           (: t (List Integer))
           (define t null)
           (: f (-> (Record t) Integer))
           (define (f x) x)))
 (run/sexp `(begin ,no-fold-sexp (f 1) (f 2) (f 3)))  3
 #:x (run/sexp `(begin ,no-fold-sexp (f 1) (f 2) (f #f)))
 "expected: (List Integer)"

 #:do (define fold-sexp
        '(begin
           (: t (Flat (check positive?)))
           (define t 1)
           (: f (-> (Record + t) Integer))
           (define (f x) x)))
 (run/sexp `(begin ,fold-sexp (f 1) (f 2) (f 3)))  3
 #:x (run/sexp `(begin ,fold-sexp (f 1) (f 2) (f -10)))
 "expected: (Flat (check positive?))"

 #:do (define re-sexp
        '(begin
           (: t (Flat (check accepting?)))
           (define t (regular-expression (seq-prefix 'a (star 'b) 'a)))
           (: f (-> (Record next t) Symbol))
           (define (f x) x)))
 (run/sexp `(begin ,re-sexp (f 'a) (f 'b) (f 'b) (f 'a)))  'a
 #:x (run/sexp `(begin ,re-sexp (f 'a) (f 'b) (f 'a) (f 'a)))
 "expected: (Flat (check accepting?))"
 )

;; pre-defined flat failure
(chk
 #:x (run/var (List 2 Boolean) x (list #t) x)
 "expected: (List 2 Boolean)"
 #:x (run/var (List Boolean) x (list 1 2) x)
 "expected: (List Boolean)"
 #:x (run/var Integer x 1/2 x)
 "expected: Integer"
 #:x (run/var Real x #t x)
 "expected: Real"
 #:x (run/var Boolean x 0 x)
 "expected: Boolean")

;; user-defined flat
(chk
 #:do (define even-sexp
        '(define-contract Even
           (Flat (domain Integer)
                 (check even?)
                 (shrink (λ (val) (floor (/ val 2))))
                 (generate (λ (fuel) (* 2 (contract-generate Integer)))))))
 (run/sexp `(begin ,even-sexp (: x Even) (define x 2) x))  2
 #:x (run/sexp `(begin ,even-sexp (: x Even) (define x 1) x))
 "expected: Even"
 #:? even?
 (run/sexp `(begin ,even-sexp (contract-generate Even)))
 (run/sexp `(begin ,even-sexp (contract-shrink Even 6)))  3
 #:x (run (contract-generate (Flat (domain Integer) (check even?))))
 "cannot generate")
|#
