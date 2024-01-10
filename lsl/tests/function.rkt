#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         lsl/private/contract/function
         lsl/private/guard
         lsl/private/proxy
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "flat.rkt" examples))

  (define pte-ctc
    (new function-contract%
         [syntax (syntax/unexpanded (-> Positive Even))]
         [domain-order '(0)]
         [domains (list (λ _ pos-ctc))]
         [codomain (λ _ even-ctc)]
         [exceptions (list)])))

;; TODO: multiple domains
;; TODO: dependent domains
;; TODO: exceptions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:do (define (dbl x) (* 2 x))

   #:? passed-guard?
   (send pte-ctc protect dbl '+)
   #:do (define dbl* ((send pte-ctc protect dbl '+) dbl '-))

   #:? failed-guard?
   (send pte-ctc protect 2 '+)
   #:x ((send pte-ctc protect 2 '+) 2 '-)
   "(-> Positive Even)"

   #:do (define fst (λ (x y) x))
   #:? failed-guard?
   (send pte-ctc protect fst '+)
   #:x ((send pte-ctc protect fst '+) fst '-)
   "given: 2-arity function"

   (dbl* 2)  4
   #:x (dbl* -1)
   "expected: Positive"

   #:? even?
   (let ([f (send pte-ctc generate 1)])
     (f 10))

   ;; TODO: shrink
   ;; TODO: interact
   ;; TODO: symbolic
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: f (-> Integer Integer))
        (define (f x) x)
        (f 10))
   10

   (run (: f (Function (arguments [x Integer]
                                  [y (Flat (check (λ (z) (eq? x z))))])
                       (result Integer)))
        (define (f x y) (+ x y))
        (f 10 10))
   20

   (run (: f (Function (arguments [x Integer])
                       (result (Flat (check (λ (y) (eq? x y)))))))
        (define (f x) x)
        (f 10))
   10

   (run (: f (Function (arguments [x Integer])
                       (result (λ (y) (eq? x y)))))
        (define (f x) x)
        (f 10))
   10

   #:? integer?
   (run ((contract-generate (-> Integer Integer)) 10))
   #:t
   (run (let ([f (contract-generate (-> Integer Integer))])
          (= (f 0) (f 0))))
   #:t
   (run* (: f (-> Integer Integer))
         (define (f x) x)
         (check-contract f))
   #:t
   (run* (: f (-> Integer Integer))
        (define (f x) x)
        (verify-contract f))
   #:t
   (run* (: f (-> Integer Integer Integer))
         (define (f x y) x)
         (check-contract f))
   #:t
   (run* (: letter-grade (-> Integer String))
         (define (letter-grade n)
           (cond [(>= n 90) "A"]
                 [(>= n 80) "B"]
                 [else "C"]))

         (: letter-grade-prop (-> Integer True))
         (define (letter-grade-prop n)
           (member? (letter-grade n) (list "A" "B" "C")))

         (verify-contract letter-grade-prop))
   #:t
   (run* (define-struct bad ())
         (: f (Function (arguments [_ (OneOf Boolean Integer)])
                        (result Integer)
                        (raises bad)))
         (define (f e)
           (if (integer? e) e (raise (make-bad))))
         (check-contract f))

   #:x (run (: f (-> Integer Boolean))
            (define (f x) x)
            (f 10))
   "expected: Boolean"

   #:x (run* (: f (-> Integer Boolean))
             (define (f x) x)
             (check-contract f))
   "expected: Boolean"

   #:x (run (: f (Function (arguments [x Integer])
                           (result (Flat (check (λ (y) (eq? x y)))))))
            (define (f x) (+ x 1))
            (f 10))
   "expected: (Flat (check (λ (y) (eq? x y))))"

   #:x (run* (define-contract Even
               (Flat
                (check even?)
                (symbolic (λ () (* 2 (contract-symbolic Integer))))))
             (: f (-> Even Even))
             (define (f x) (+ x 1))
             (verify-contract f))
   "expected: Even"

   #:x (run* (: bad-mult (-> Real Real Real))
             (define (bad-mult x y)
               (if (= x 10417)
                   0
                   (* x y)))
             (: bad-mult-prop (-> Real Real True))
             (define (bad-mult-prop x y)
               (= (bad-mult x y)
                  (* x y)))
             (verify-contract bad-mult-prop))
   "counterexample: (bad-mult-prop 10417.0"

   #:x (run* (define-struct bad ())
             (: f (Function (arguments [_ (OneOf Boolean Integer)])
                            (result Integer)))
             (define (f e)
               (if (integer? e) e (raise (make-bad))))
             (check-contract f 20))
  "exception raised: (bad)"

  #:x (run (: f (Function (arguments [x (Flat (check (λ (z) (eq? y z))))]
                                     [y (Flat (check (λ (z) (eq? x z))))])
                          (result Integer)))
           (define (f x y) (+ x y))
           (f 10 20))
  "cannot have cyclic dependency"

  #:x (run* (: f (-> Integer Integer))
            (define (f x y) x))
  "expected: 1-arity function"

  #:do (define fb
         '(define-contract FizzBuzz
            (Flat
             #;(domain Integer)
             (check (lambda (x) (not (or (zero? (modulo x 3)) (zero? (modulo x 5))))))
             (generate (λ () (+ (* 15 (contract-generate Integer)) 1))))))

  (run/sexp fb '(: x FizzBuzz) '(define x 4) 'x)
  4

  #:t
  (run/sexp fb
            '(: f (-> (-> Integer Integer) FizzBuzz))
            '(define (f g) 4)
            '(check-contract f)
            #:no-result #t)

  #;(run/sexp fb
            (: f (-> Integer FizzBuzz))
            (define (f x) (+ (* 15 x) 1))
            (verify-contract f))
  #;(void)

  #:x (run/sexp fb '(: x FizzBuzz) '(define x 3) 'x)
  "expected: FizzBuzz"

  ;#:x (run/sexp fb '(: f (-> Integer FizzBuzz)) '(define (f x) x) '(verify-contract f))
  ;"expected: FizzBuzz"
  ))
