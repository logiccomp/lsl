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
   "expected: boolean?"

   #:x
   (run* (: id (All (A) (-> A Any)))
         (define (id x) x)
         (define y (id 10))
         (cond [y y] [else y]))
   "expected: boolean?"

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
   "expected: number?"

   (run (define-struct foo [x])
        (: f (-> (Struct foo [Integer]) (Struct foo [Integer])))
        (define (f x) x)
        (foo-x (f (make-foo 10))))
   10

   #:t
   (run* (define-struct leaf [value])
         (define-struct node [left right])
         (define-contract (Leaf X) (Struct leaf [X]))
         (define-contract (Node X Y) (Struct node [X Y]))
         (define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))
         (: tree-map (All (X Y) (-> (-> X Y) (Tree X) (Tree Y))))
         (define (tree-map f t)
           (cond [(leaf? t) (make-leaf (f (leaf-value t)))]
                 [(node? t) (make-node (tree-map f (node-left t))
                                       (tree-map f (node-right t)))]))
         (check-expect (tree-map number->string (make-node (make-node (make-leaf 1) (make-leaf 2)) (make-leaf 3)))
                       (make-node (make-node (make-leaf "1") (make-leaf "2")) (make-leaf "3"))))

   #:x
   (run* (define-package nat-counter #f))
   "define-package: unknown contract"

   #:x
   (run* (: nat-counter Natural)
         (define-package nat-counter #f))
   "not a package contract"

   #:x
   (run* (: thing (Exists (T) (List T)))
         (define-package thing #f))
   "expected: (List T)"

   (run (define-struct counter-pkg (make incr get))

        (define-contract Counter
          (Exists (T)
            (Struct counter-pkg
                    [(-> T)
                     (-> T T)
                     (-> T Natural)])))

        (: nat-counter Counter)
        (define-package nat-counter
          (make-counter-pkg
           (λ () '())
           (λ (x) (cons 'A x))
           (λ (x) (length x))))

        (: f (-> NatCounter NatCounter))
        (define (f x) (nat-counter-incr x))

        (nat-counter-get (f (nat-counter-incr (nat-counter-make)))))
   2

   #:x
   (run* (define-struct counter-pkg (make))
         (define-contract Counter
           (Exists (T) (Struct counter-pkg [(-> T)])))

         (: nat-counter Counter)
         (define-package nat-counter
           (make-counter-pkg
            (λ () #f)))

         (: f (-> NatCounter Any))
         (define (f x) x)

         (f #f))
   "expected: NatCounter"

   ;; TODO: should be consistent with the above
   #:x
   (run* (define-struct counter-pkg (f))
         (define-contract Counter
           (Exists (T) (Struct counter-pkg [(-> T Any)])))

         (: nat-counter Counter)
         (define-package nat-counter
           (make-counter-pkg
            (λ (x) x)))

         (nat-counter-f #f))
   "expected: #<NatCounter>"
   ))
