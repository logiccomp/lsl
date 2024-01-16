#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         racket/match
         lsl/private/contract/list
         lsl/private/guard
         lsl/private/util
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (define blist-ctc
    (new list-contract%
         [syntax (syntax/unexpanded (List Boolean))]
         [fixed? #f]
         [contracts (list bool-ctc)]))

  (define pair-ctc
    (new list-contract%
         [syntax (syntax/unexpanded (Tuple Even Even))]
         [fixed? #t]
         [contracts (list even-ctc even-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send blist-ctc protect '(#t #t) #f)
   ((send blist-ctc protect '(#t #t) #f) '(#t #t) #f)  '(#t #t)

   #:? failed-guard?
   (send blist-ctc protect '(#t 42) #f)
   #:x ((send blist-ctc protect '(#t 42) #f) '(#t 42) #f)
   "expected: Boolean"

   #:? passed-guard?
   (send pair-ctc protect '(2 4) #f)
   ((send pair-ctc protect '(2 4) #f) '(2 4) #f)  '(2 4)

   #:? failed-guard?
   (send pair-ctc protect '(1) #f)
   #:x ((send pair-ctc protect '(1) #f) '(1) #f)
   "expected: (Tuple Even Even)"
   #:x ((send pair-ctc protect '(42 13) #f) '(42 13) #f)
   "given: 13"

   #:t (andmap boolean? (send blist-ctc generate 20))
   #:t (match (send pair-ctc generate 20)
         [(list x y) (and (even? x) (even? y))]
         [_ #f])

   #:t (andmap boolean? (send blist-ctc shrink 20 '(#t #t #t #t)))
   (send pair-ctc shrink 20 '(2 4))  '(0 2)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x (List Boolean)) (define x (list #t #f)) x)  (list #t #f)
   #:x (run (: x (List Boolean)) (define x 3) x)  "expected: (List Boolean)"
   #:x (run (: x (List Boolean)) (define x (list #t 3 #f)) x)  "expected: Boolean"

   (run (: x (Tuple Integer Boolean)) (define x (list 3 #f)) x)  (list 3 #f)
   #:x (run (: x (Tuple Integer Boolean)) (define x 3) x)  "expected: (Tuple Integer Boolean)"
   #:x (run (: x (Tuple Integer Boolean)) (define x (list 1 #t 1)) x)  "expected: (Tuple Integer Boolean)"
   #:x (run (: x (Tuple Integer Boolean)) (define x (list 3 3)) x)  "expected: Boolean"

   (run (contract-shrink (List Integer) '(0) 20))  '()

   #:? (λ (xs) (andmap integer? xs))
   (run (contract-generate (List Integer)))
   ))
