#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   #:t (run (: f (-> (AllOf) Any)) (define (f x) 10) #t)
   #:t (run (define-struct foo (x))
            (: f (-> (Foo Integer) (Foo Integer)))
            (define (f x) x)
            (and (equal? (make-foo 10) (make-foo 10))
                 (equal? (f (make-foo 10)) (f (make-foo 10)))
                 (equal? (f (f (make-foo 10))) (make-foo 10))
                 (equal? (make-foo 10) (f (f (make-foo 10))))
                 (not (equal? (make-foo 10) (make-foo 11)))
                 (not (equal? (f (make-foo 10)) (f (make-foo 11))))
                 (not (equal? (f (f (make-foo 10))) (make-foo 11)))
                 (not (equal? (make-foo 10) (f (f (make-foo 11)))))))
   #:t (run (define-contract (Nonempty X)
               (Immediate
                (check (lambda (x) (and (list? x) (> (length x) 0))))
                (generate (lambda (fuel) (list (contract-generate X fuel))))))

             (: xs (Exists (Y) (Nonempty Y)))
             (define xs '(1))
             #t)
   #:t (run* (: f (-> (OneOf (Tuple (OneOf Integer Boolean))) True))
             (define (f p)
               #t)
             (verify-contract f))
   #:t (run* (define-struct add [])
             (: f (-> (OneOf Integer (Add)) Integer))
             (define (f p)
               (cond [(add? p) 0]
                     [(integer? p) 0]))
             (verify-contract f))
   #:t (run* (define-struct push [num])
             (define-struct add [])
             (: f (-> (Tuple (OneOf (Push Integer) (Add))) Any))
             (define (f instrs)
               (cond [(add? (first instrs)) 0]
                     [(push? (first instrs)) (push-num (first instrs))]))
             (verify-contract f))
   #:t (run* (define-struct add [])
             (: f (-> (OneOf (Tuple) (Tuple (Add))) (List (Add))))
             (define (f p) p)
             (verify-contract f))
   #:t (run* (define-struct push [num])
             (define-struct add [])
             (define-struct mul [])
             (define-struct sub [])
             (define-contract SimpleInstr (OneOf (Push Integer) (Add) (Mul) (Sub)))
             (: g (-> (List SimpleInstr) (List SimpleInstr)))
             (define (g p) p)
             (: f (-> (Tuple SimpleInstr) (List SimpleInstr)))
             (define (f p) (g p))
             (verify-contract f))
   #:t (run* (define-struct foo ())
             (define (f) (raise (make-foo)))
             (check-raises (f))
             (check-raises (f) foo))
   #:x (run* (define-struct foo (x))
             (check-expect (make-foo 10) (make-foo 11))
             #t)
   "FAILURE"
   #:x (run (define-struct foo (x))
            (define-struct bar (x y))
            (: f (-> (Foo Integer) Any))
            (define (f x) x)
            (bar-y (f (make-foo 10))))
   "expected: bar?"
   #:x (run (define-contract MyContract ...)
            (: foo (-> Any MyContract))
            (define (foo x) x)
            (foo 10))
   "expected a finished contract, but found a template"
   #:x (run* (define-struct posn (x y))
             (: f (-> (List (Posn Natural Natural)) Any))
             (define (f m)
               (if (empty? m) '()
                   (let* ([max-x (apply max (map posn-x m))]
                          [max-y (apply max (map posn-y m))]
                          [p (make-posn max-x max-y)])
                     (first (filter (lambda (a) (equal? a p)) m)))))
             (check-contract f))
   "first: contract violation"
   #:x (run* (: f (-> (AllOf Integer (Constant 0)) String))
             (define (f x) x)
             (check-contract f))
   "counterexample: (f 0)"
   #:x (run* (check-error (raise "blah") 20))
   "check-error: contract violation"
   #:x (run* (string=? 'hi "hi"))
   "string=?: contract violation"
   #:x (run* (: g (-> Integer String))
             (define (g x) x)
             (: f (-> Integer Integer))
             (define (f x) (g x))
             (check-contract f))
   "expected: String"
   #:x (run* (: f (-> Any Any Any))
             (define (f x) x))
   "given: 1-arity function"
   #:x (run* (: f (All (X) (-> (List X) Any)))
             (define (f xs)
               (ormap identity xs))
             (f '(1 2 3)))
   "expected: boolean?"
   #:x (run* (if 1 2 3))
   "expected: boolean?"
   #:t
   (run (define-struct foo (v))
        (define x (make-foo 1))
        (: L (List Any))
        (define L empty)
        (: f (-> (Foo Any) (Record L)))
        (define (f x)
          (begin (set-foo-v! x 2)
                 x))
        (f x)
        (f x)
        (eq? (first L) (second L)))
   #:t
   (run (: A (Action Natural))
        (define A (action 5 (list)))
        A)
   #:x (run (: x Integer)
            (define x 10)
            (set! x ""))
   "expected: Integer"
   #:x (run (: f (All (X Y) (-> X Y)))
            (define (f x) x)
            (set! f (lambda (x) x))
            (f 10))
   "expected: ∀Y"
   ))
