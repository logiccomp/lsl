#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         racket/list
         racket/match
         racket/promise
         lsl/private/contract/struct
         lsl/private/contract/oneof
         lsl/private/contract/recursive
         lsl/private/guard
         lsl/private/proxy
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (struct thing root (v)
    #:transparent
    #:mutable)

  (define (unthing x)
    (if (thing? x) (unthing (thing-v x)) x))

  (define rec-even-thing-ctc
    (new recursive-contract%
         [syntax (syntax/unexpanded (Recursive (X) (OneOf Even (Thing X))))]
         [promise (delay even-thing-ctc)]))

  (define thing-ctc
    (new struct-contract%
       [syntax (syntax/unexpanded (Thing X))]
       [constructor thing]
       [predicate thing?]
       [accessors (list thing-v)]
       [mutators (list set-thing-v!)]
       [contracts (list rec-even-thing-ctc)]))

  (define even-thing-ctc
    (new oneof-contract%
         [syntax (syntax/unexpanded (OneOf Even (Thing X)))]
         [disjuncts (list even-ctc thing-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:do (define good-thing (thing (thing (thing 10))))
   #:? passed-guard?
   (send rec-even-thing-ctc protect good-thing '+)
   ((send rec-even-thing-ctc protect good-thing '+) good-thing '-)  good-thing

   #:do (define bad-thing (thing (thing (thing 5))))
   #:? failed-guard?
   (send rec-even-thing-ctc protect bad-thing '+)
   #:x ((send rec-even-thing-ctc protect bad-thing '+) bad-thing '-)
   "expected: (OneOf Even (Thing X))"

   #:do (define (two-thing? x)
          (match x
            [(thing (thing _)) #t]
            [_ #f]))
   #:t
   (let ([xs (map (λ _ (send rec-even-thing-ctc generate 5)) (range 20))])
     (and (andmap (or/c thing? (and/c integer? even?)) xs)
          (findf two-thing? xs)
          (andmap even? (map unthing xs))))

   (send rec-even-thing-ctc shrink 1 good-thing)
   (thing (thing (thing 5)))

   ;; TODO: shrink depth of data
   ;; TODO: interact
   ;; TODO: symbolic
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   #:do (define node '(define-struct node (left value right)))
   #:do (define tree-sexp
          '(define-contract (Tree X)
             (OneOf X (Node (Tree X) X (Tree X)))))
   #:do (define sum-sexp
          '(define (sum x)
             (if (integer? x)
                 x
                 (+ (sum (node-left x))
                    (node-value x)
                    (sum (node-right x))))))

   (run/sexp node
             '(define-contract IntTree
               (OneOf Integer (Node IntTree Integer IntTree)))
             '(: sum (-> IntTree Integer))
             sum-sexp
             '(sum (make-node (make-node 1 2 3) 0 (make-node (make-node 4 5 6) 7 8))))
   36
   (run/sexp node tree-sexp
             '(: sum (-> (Tree Integer) Integer))
             sum-sexp
             '(sum (make-node (make-node 1 2 3) 0 (make-node (make-node 4 5 6) 7 8))))
   36

   (run/sexp node tree-sexp sum-sexp
             '(sum (contract-shrink (Tree Integer) (make-node 10 20 (make-node 1 2 3)))))
   17

   #:t
   (for/and ([_ (in-range 10)])
     (run/sexp node tree-sexp
               '(define (int-tree? x)
                  (or (integer? x)
                      (and (int-tree? (node-left x))
                           (integer? (node-value x))
                           (int-tree? (node-right x)))))
               '(int-tree? (contract-generate (Tree Integer)))))

   #:x
   (run/sexp node
             '(define-contract IntTree
               (OneOf Integer (Node IntTree Integer IntTree)))
             '(: sum (-> IntTree Integer))
             sum-sexp
             '(sum (make-node (make-node 1 2 #f) 0 (make-node (make-node 4 5 6) 7 8))))
   "contract violation"
   #:x
   (run/sexp node tree-sexp
             '(: sum (-> (Tree Integer) Integer))
             sum-sexp
             '(sum (make-node (make-node 1 2 #f) 0 (make-node (make-node 4 5 6) 7 8))))
   "contract violation"
   #:x
   (run/sexp node
             '(define-contract (Tree X)
               (OneOf X (Node (Tree Boolean) X (Tree X))))
             '(: sum (-> (Tree Integer) Integer))
             sum-sexp)
   "must be exactly (Tree Integer)"
   ))
