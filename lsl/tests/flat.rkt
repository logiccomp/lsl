#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
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
         [syntax (syntax/unexpanded Boolean)]
         [checker boolean?]
         [generator (λ (fuel) (< (random) 1/2))]
         [shrinker (λ (fuel val) #f)]))

  (define pos-ctc
    (new flat-contract%
         [syntax (syntax/unexpanded Positive)]
         [checker (λ (x) (and (real? x) (positive? x)))]
         [generator (λ (fuel) (random fuel))]
         [shrinker (λ (fuel val) (floor (/ val 2)))]))

  (define even-ctc
    (new flat-contract%
         [syntax (syntax/unexpanded Even)]
         [checker (λ (x) (and (integer? x) (even? x)))]
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

   (send even-ctc interact 'generate 1 #f)  #f
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x Integer) (define x 10) x)  10
   #:x (run (: x Integer) (define x 1/2) x)  "expected: Integer"
   #:? integer?
   (run (contract-generate Integer))

   (run (: x Real) (define x 3.14) x)  3.14
   #:x (run (: x Real) (define x #t) x)  "given: #t"
   #:? real?
   (run (contract-generate Real))

   (run (: x Boolean) (define x #t) x)  #t
   #:x (run (: x Boolean) (define x 0) x)  "(as server)"
   #:? boolean?
   (run (contract-generate Boolean))

   #:do (define even-sexp
          '(define-contract Even
             (Flat (check (λ (x) (and (integer? x) (even? x))))
                   (generate (λ (fuel) (* 2 (contract-generate Integer))))
                   (shrink (λ (fuel val) (floor (/ val 2))))
                   (symbolic (λ () (* 2 (contract-symbolic Integer)))))))
   (run/sexp `(begin ,even-sexp (: x Even) (define x 2) x))  2
   #:x (run/sexp `(begin ,even-sexp (: x Even) (define x 1) x))  "expected: Even"
   #:? even?
   (run/sexp `(begin ,even-sexp (contract-generate Even)))
   (run/sexp `(begin ,even-sexp (contract-shrink Even 6)))  3
   #:x (run (contract-generate (Flat (check even?))))  "contract-generate: failed"
   ))
