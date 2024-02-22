#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         lsl/private/contract/struct
         lsl/private/guard
         lsl/private/proxy
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (struct eb root (e b)
    #:transparent
    #:mutable
    #:methods gen:equatable
    [(define (base-equal? self other)
       (and (equal? (eb-e self) (eb-e other))
            (equal? (eb-b self) (eb-b other))))])

  (define eb-struct-ctc
    (new struct-contract%
         [syntax (syntax/unexpanded (Thing Even Boolean))]
         [constructor eb]
         [predicate eb?]
         [accessors (list eb-e eb-b)]
         [mutators (list set-eb-e! set-eb-b!)]
         [contracts (list even-ctc bool-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send eb-struct-ctc protect (eb 2 #f) '+)
   ((send eb-struct-ctc protect (eb 2 #f) '+) (eb 2 #f) '-)  (eb 2 #f)

   #:? failed-guard?
   (send eb-struct-ctc protect (eb 2 2) '+)
   #:x ((send eb-struct-ctc protect (eb 2 2) '+) (eb 2 2) '-)
   "expected: Boolean"

   #:? failed-guard?
   (send eb-struct-ctc protect (eb 3 #f) '+)
   #:x ((send eb-struct-ctc protect (eb 3 #f) '+) (eb 3 #f) '-)
   "expected: Even"

   #:t
   (let ([x (send eb-struct-ctc generate 1)])
     (and (even? (eb-e x))
          (boolean? (eb-b x))))

   (send eb-struct-ctc shrink 1 (eb 4 #t))  (eb 2 #f)

   ;; TODO: interact
   ;; TODO: symbolic
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (define-struct foo (x))
        (format "~v" (make-foo 7)))
   "(make-foo 7)"

   (run (define-struct foo (x))
        (: f (-> (Foo Integer) Integer))
        (define (f st) (foo-x st))
        (f (make-foo 10)))
   10

   #:t
   (run (define-struct foo (x))
        (: f (-> Any (Foo Integer)))
        (define (f x) x)
        (define orig (make-foo 10))
        (define prox (f orig))
        (set-foo-x! prox 17)
        (and (= (foo-x orig) (foo-x prox) 17)
             (equal? orig prox)
             (equal? prox orig)
             (equal? (format "~a" prox)
                     (format "~a" orig))))

   #:? integer?
   (run (define-struct foo (x))
        (foo-x (contract-generate (Foo Integer))))

   (run (define-struct foo (x))
        (foo-x (contract-shrink (Foo Integer) (make-foo 10))))
   5

   #:x
   (run (define-struct foo (x))
        (: f (-> Any (Foo Integer)))
        (define (f x) x)
        (set-foo-x! (f (make-foo 10)) ""))
   "expected: Integer"

   #:x
   (run (define-struct foo (x))
        (: f (-> Any (Foo Integer)))
        (define (f x) x)
        (define orig (make-foo 10))
        (define prox (f orig))
        (set-foo-x! orig "")
        (foo-x prox))
   "expected: Integer"

   #:x
   (run (define-struct foo (x))
        (: f (-> (Foo Integer) Integer))
        (define (f st) (foo-x st))
        (f (make-foo 1/2)))
   "expected: Integer"

   #:t
   (run* (define-struct foo (x))
         (: f (-> (Foo Integer) True))
         (define (f st) (= (foo-x st) (foo-x st)))
         (verify-contract f))

   #:x
   (run* (define-struct foo (x))
         (: f (-> (Foo Integer) True))
         (define (f st) (zero? (foo-x st)))
         (verify-contract f))
   "counterexample: (f (make-foo 1))"

   #:x (run* (define-struct foo (x y))
             (check-expect (make-foo 1 2) #t))
   "(make-foo 1 2)"
   ))
