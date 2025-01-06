#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(define (make-password-sexp a b)
  `[(define CORRECT-PASSWORD
      (explode "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"))

    (define-contract Password
      (lambda (s) (and (list? s) (andmap string? s))))

    (: password=? (-> Password Password Boolean))
    (define (password=? l1 l2)
      (cond [(and (empty? l1) (empty? l2)) #t]
            [(and (empty? l1) (cons? l2)) #f]
            [(and (cons? l1) (empty? l2)) #f]
            [(and (cons? l1) (cons? l2))
             (and (string=? (first l1) (first l2))
                  (password=? (rest l1) (rest l2)))]))

    (: check-password (-> Password Boolean))
    (define (check-password p)
      (password=? CORRECT-PASSWORD p))

    (distinguishable?
     (lambda () (check-password ,a))
     (lambda () (check-password ,b)))])

(module+ test
  (chk
   (apply run/sexp (make-password-sexp 'CORRECT-PASSWORD 'CORRECT-PASSWORD))
   #f

   (apply run/sexp (make-password-sexp 'CORRECT-PASSWORD 'empty))
   #t))
