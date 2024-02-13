#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: id (All (A) (-> A A)))
        (define (id x) x)
        (list (id 1) (id "foo")))
   '(1 "foo")

   #:t
   (run* (: id (All (A) (-> A A)))
         (define (id x) x)
         (check-contract id))

   #:x
   (run (: id (All (A) (-> A A)))
        (define (id x) 1)
        (id "foo"))
   "expected: ∀A"

   #:x
   (run* (: id (All (A) (-> A A)))
         (define (id x) 1)
         (check-contract id))
   "expected: ∀A"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (if y y y))
   "cannot use parametric value"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (cond [y y] [else y]))
   "cannot use parametric value"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (check-expect y y))
   "cannot use parametric value"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (equal? y y))
   "expected: non-parametric?"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (number? y))
   "expected: non-parametric?"

   (run (: counter-pkg (Exists (A) (Tuple (-> A) (-> A A) (-> A Integer))))
        (define counter-pkg
          (list (λ () 0)
                (λ (x) (+ x 1))
                (λ (x) x)))
        (define make-counter (first counter-pkg))
        (define counter-incr (second counter-pkg))
        (define counter-get (third counter-pkg))
        (counter-get (counter-incr (counter-incr (make-counter)))))
   2

   (run (: pkg (Exists (A) (Tuple (-> A Integer) A)))
        (define pkg (list identity 3))
        (: unpkg (-> (Exists (A) (Tuple (-> A Integer) A)) Integer))
        (define (unpkg p)
          ((first p) (second p)))
        (list ((first pkg) (second pkg))
              (unpkg pkg)))
   '(3 3)

   #:x
   (run (: make-counter (Exists (A) (-> A)))
        (define (make-counter) 0)
        (+ 1 (make-counter)))
   "expected real?"

   (run (define-struct foo [x])
        (: f (-> (Foo Integer) (Foo Integer)))
        (define (f x) x)
        (foo-x (f (make-foo 10))))
   10

   #:t
   (run* (define-struct leaf [value])
         (define-struct node [left right])
         (define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))
         (: tree-map (All (X Y) (-> (-> X Y) (Tree X) (Tree Y))))
         (define (tree-map f t)
           (cond [(leaf? t) (make-leaf (f (leaf-value t)))]
                 [(node? t) (make-node (tree-map f (node-left t))
                                       (tree-map f (node-right t)))]))
         (check-expect (tree-map number->string (make-node (make-node (make-leaf 1) (make-leaf 2)) (make-leaf 3)))
                       (make-node (make-node (make-leaf "1") (make-leaf "2")) (make-leaf "3"))))
   ))
