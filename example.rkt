#lang lsl

;; Attaching a contract
(: favorite-number Integer)
(define favorite-number 13)

;; Defining a contract
(define-contract MyInteger Integer)
(: another-number MyInteger)
(define another-number 0)

;; Function contract
(: square (function (arguments [in Integer]) (results [out Integer])))
(define (square x)
  (* x x))

;; Custom contract
(define-contract Even
  (flat
   (domain Integer)
   (check even?)
   (generate (λ () (* 2 (contract-generate Integer))))))

;; Shorthand contract
(: add-one (-> Even Even))
(define (add-one x)
  (+ x 1))

;; Fails random generation
;(contract-exercise add-one)

;; Fails verification
;(contract-verify add-one)
