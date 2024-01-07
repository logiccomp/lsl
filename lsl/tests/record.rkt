#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   #:do (define no-fold-sexp
          '(begin
             (: t (List Integer))
             (define t null)
             (: f (-> (Record t) Integer))
             (define (f x) x)))
   (run/sexp `(begin ,no-fold-sexp (f 1) (f 2) (f 3)))  3
   #:x (run/sexp `(begin ,no-fold-sexp (f 1) (f 2) (f #f)))
   "expected: Integer"

   #:do (define fold-sexp
          '(begin
             (: t (Flat (check positive?)))
             (define t 1)
             (: f (-> (Record + t) Integer))
             (define (f x) x)))
   (run/sexp `(begin ,fold-sexp (f 1) (f 2) (f 3)))  3
   #:x (run/sexp `(begin ,fold-sexp (f 1) (f 2) (f -10)))
   "expected: (Flat (check positive?))"

   #:do (define re-sexp
          '(begin
             (: t (Flat (check machine-accepting?)))
             (define t (re (seq-prefix 'a (star 'b) 'a)))
             (: f (-> (Record machine-next t) Symbol))
             (define (f x) x)))
   (run/sexp `(begin ,re-sexp (f 'a) (f 'b) (f 'b) (f 'a)))  'a
   #:x (run/sexp `(begin ,re-sexp (f 'a) (f 'b) (f 'a) (f 'a)))
   "expected: (Flat (check machine-accepting?))"
   ))
