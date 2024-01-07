#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         lsl/private/contract/allof
         lsl/private/guard
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "flat.rkt" examples))

  (define pos-even-ctc
    (new allof-contract%
         [syntax (syntax/unexpanded PositiveEven)]
         [conjuncts (list even-ctc pos-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send pos-even-ctc protect 2 '+)
   ((send pos-even-ctc protect 2 '+) 2 '-)  2

   #:? failed-guard?
   (send pos-even-ctc protect -2 '+)
   #:x ((send pos-even-ctc protect -2 '+) -2 '-)
   "expected: Positive"

   #:? failed-guard?
   (send pos-even-ctc protect 3 '+)
   #:x ((send pos-even-ctc protect 3 '+) 3 '-)
   "expected: Even"

   #:? (and/c even? positive?)
   (send pos-even-ctc generate 20)
   (send pos-even-ctc shrink 20 6)  2
   ;; TODO: shrinking where "none replacement" is needed

   ;; TODO: interact
   ;; TODO: symbolic
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x (AllOf Integer (Flat (check positive?)))) (define x 10) x)  10
   #:x (run (: x (AllOf Integer (Flat (check positive?)))) (define x #f) x)
   "expected: Integer"
   #:x (run (: x (AllOf Integer (Flat (check positive?)))) (define x -10) x)
   "expected: (Flat (check positive?))"

   #:? (and/c integer? positive?)
   (run (contract-generate (AllOf Integer (Flat (check positive?)))))
   (run (contract-shrink (AllOf (Flat (check positive?)) Integer) 10))  5
   ))
