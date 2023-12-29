#lang racket/base

(require chk
         "util.rkt")

;; recursive

(chk
 #:do (define node '(define-struct node (left value right)))
 #:do (define sum-sexp
        '(define (sum x)
           (if (integer? x)
               x
               (+ (sum (node-left x))
                  (node-value x)
                  (sum (node-right x))))))

 ;; success
 (run/sexp `(begin ,node
                   (define-contract IntTree
                     (OneOf Integer (Node IntTree Integer IntTree)))
                   (: sum (-> IntTree Integer))
                   ,sum-sexp
                   (sum (make-node (make-node 1 2 3) 0 (make-node (make-node 4 5 6) 7 8)))))
 36
 (run/sexp `(begin ,node
                   (define-contract (Tree X)
                     (OneOf X (Node (Tree X) X (Tree X))))
                   (: sum (-> (Tree Integer) Integer))
                   ,sum-sexp
                   (sum (make-node (make-node 1 2 3) 0 (make-node (make-node 4 5 6) 7 8)))))
 36

 #:t
 (for/and ([_ (in-range 10)])
   (run/sexp `(begin ,node
                     (define-contract (Tree X)
                       (OneOf X (Node (Tree X) X (Tree X))))
                     (define (int-tree? x)
                       (or (integer? x)
                           (and (int-tree? (node-left x))
                                (integer? (node-value x))
                                (int-tree? (node-right x)))))
                     (int-tree? (contract-generate (Tree Integer))))))

 ;; failure
 #:x
 (run/sexp `(begin ,node
                   (define-contract IntTree
                     (OneOf Integer (Node IntTree Integer IntTree)))
                   (: sum (-> IntTree Integer))
                   ,sum-sexp
                   (sum (make-node (make-node 1 2 #f) 0 (make-node (make-node 4 5 6) 7 8)))))
 "contract violation"
 #:x
 (run/sexp `(begin ,node
                   (define-contract (Tree X)
                     (OneOf X (Node (Tree X) X (Tree X))))
                   (: sum (-> (Tree Integer) Integer))
                   ,sum-sexp
                   (sum (make-node (make-node 1 2 #f) 0 (make-node (make-node 4 5 6) 7 8)))))
 "contract violation"
 #:x
 (run/sexp `(begin ,node
                   (define-contract (Tree X)
                     (OneOf X (Node (Tree Boolean) X (Tree X))))
                   (: sum (-> (Tree Integer) Integer))
                   ,sum-sexp))
 "must be exactly (Tree Integer)"
 )
