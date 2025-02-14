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
   (run/sexp no-fold-sexp '(f 1) '(f 2) '(f 3))  3
   #:x (run/sexp no-fold-sexp '(f 1) '(f 2) '(f #f))
   "expected: Integer"

   #:do (define fold-sexp
          '(begin
             (: t (Immediate (check positive?)))
             (define t 1)
             (: f (-> (Record + t) Integer))
             (define (f x) x)))
   (run/sexp fold-sexp '(f 1) '(f 2) '(f 3))  3
   #:x (run/sexp fold-sexp '(f 1) '(f 2) '(f -10))
   "expected: (Immediate (check positive?))"

   #:do (define re-sexp
          '(begin
             (: t (Immediate (check machine-accepting?)))
             (define t (re (seq-prefix 'a (star 'b) 'a)))
             (: f (-> (Record machine-next t) Symbol))
             (define (f x) x)))
   (run/sexp re-sexp '(f 'a) '(f 'b) '(f 'b) '(f 'a))  'a
   #:x (run/sexp re-sexp '(f 'a) '(f 'b) '(f 'a) '(f 'a))
   "expected: (Immediate (check machine-accepting?))"

   #:x (run* (require racket/list)
             (define-contract UniqueList
               (lambda (l)
                 (equal? (length l)
                         (length (remove-duplicates l)))))
             (: ids UniqueList)
             (define ids empty)
             (: maybe-unique2 (-> (AllOf Natural (Record ids))))
             (define (maybe-unique2)
               (random 1000))
             (check-contract maybe-unique2 1000))
   "expected: UniqueList"
   ))
