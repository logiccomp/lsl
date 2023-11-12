#lang racket/base

;;
;; provide
;;

(provide
 (rename-out
  [mb #%module-begin]))

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     errortrace/errortrace-lib))

;;
;; module begin
;;


(define-syntax mb
  (syntax-parser
    [(_ . body)
     (define stx #`(module . #,(strip-context #`(_ lsl/lang . body))))
     (syntax-parse (errortrace-annotate stx)
       [(_ _ _ (_ . body))
        #`(#%plain-module-begin
           (require (only-in lsl/lang)
                    (only-in errortrace))
           . body)])]))

;;
;; reader
;;

(module reader syntax/module-reader
  lsl)
