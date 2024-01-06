#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         racket/list
         lsl/private/contract/oneof
         lsl/private/guard
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "flat.rkt" examples))

  (define even-or-bool-ctc
    (new oneof-contract%
         [syntax #'_]
         [disjuncts (list even-ctc bool-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send even-or-bool-ctc protect 2 '+)
   ((send even-or-bool-ctc protect 2 '+) 2 '-)  2

   #:? passed-guard?
   (send even-or-bool-ctc protect #t '+)
   ((send even-or-bool-ctc protect #t '+) #t '-)  #t

   #:? failed-guard?
   (send even-or-bool-ctc protect 3 '+)
   #:x ((send even-or-bool-ctc protect 3 '+) 3 '-)
   "TODO"

   #:t
   (let ([xs (map (λ _ (send even-or-bool-ctc generate 1)) (range 20))])
     (and (andmap (or/c (and/c integer? even?) boolean?) xs)
          (findf even? xs)
          (findf boolean? xs)))
   (send even-or-bool-ctc shrink 1 6)  2
   (send even-or-bool-ctc shrink 1 #t)  #f

   ;; TODO: interact
   ;; TODO: symbolic
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

#|

;; success
(chk
 (run/var (OneOf Integer Boolean) x 10 x)  10
 (run/var (OneOf Integer Boolean) x #t x)  #t
 (run (contract-shrink (OneOf (Constant -10) Integer) 10))  5
 (run (contract-shrink (List Integer) '(0) 20))  '()
 #:? (or/c 0 1)
 (run (contract-generate (OneOf (Constant 0) (Constant 1)))))

;; failure
(chk
 #:x (run/var (OneOf Integer Boolean) x 1/2 x)
 "expected: (OneOf Integer Boolean)")

|#
