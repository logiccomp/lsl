#lang racket/base

;;
;; provide
;;

(provide (for-syntax free-variables))

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse
                     syntax/id-set))

;;
;; free variables
;;

(begin-for-syntax
  (define current-free-variables (make-parameter #f))

  (define (free-variables stx)
    (define fv-set (mutable-bound-id-set))
    (define stx*
      #`(let-syntax ([#,(datum->syntax stx '#%top)
                      (make-rename-transformer #'top-collect)])
          #,stx))
    (parameterize ([current-free-variables fv-set])
      (local-expand stx* 'expression null)
      (bound-id-set->list fv-set))))

(define-syntax top-collect
  (syntax-parser
    [(_ . id)
     (bound-id-set-add! (current-free-variables) #'id)
     #''_]))
