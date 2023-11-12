#lang lsl

;; Attaching a contract
(: favorite-number Integer)
(define favorite-number 13)

;; Defining a contract
(define-contract MyInteger Integer)
(: another-number MyInteger)
(define another-number 0)

;; Function contract
(: square (function [in Integer] Integer))
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
;;(contract-verify add-one)

(: add-two (-> Even Even))
(define (add-two x)
  (+ x 2))

;(contract-verify add-two)

(: f (-> Integer Integer))
(define (f x) #t)

(: g (-> Integer Integer))
(define (g x) (f x))

;; fails random generation
;(contract-exercise f)

;(contract-verify f)

(define-contract FizzBuzz
  (flat
   (domain Integer)
   (check (lambda (x) (not (or (zero? (modulo x 3)) (zero? (modulo x 5))))))
   (generate (λ () (+ (* 15 (contract-generate Integer)) 1)))))

; fails validation
;(: a-good-number FizzBuzz)
(define a-good-number 3)

(: h (-> Integer FizzBuzz))
(define (h x) x)
; fails verification
;(contract-verify h)


(: myfn (-> Integer Boolean))
(define (myfn x) x)

; (contract-verify myfn)
