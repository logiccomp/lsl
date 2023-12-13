#lang racket/base

(require chk
         "util.rkt")

;; recursive

(chk
 #:do (define int-tree-sexp
        '[(define-struct node (left value right))
          (define-contract IntTree
            (OneOf Integer (Node IntTree Integer IntTree)))
          (: sum (-> IntTree Integer))
          (define (sum x)
            (if (integer? x)
                x
                (+ (sum (node-left x))
                   (node-value x)
                   (sum (node-right x)))))])

 ;; success
 (run/sexp
  `(begin ,@int-tree-sexp
          (sum (node (node 1 2 3) 0 (node (node 4 5 6) 7 8)))))
 36

 ;; failure
 #:x
 (run/sexp
  `(begin ,@int-tree-sexp
          (sum (node (node 1 2 #f) 0 (node (node 4 5 6) 7 8)))))
 "contract violation"
 )
