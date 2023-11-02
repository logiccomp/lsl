#lang racket/base

;;
;; provide
;;

(provide (struct-out blame-struct)
         (struct-out positive-blame-struct)
         (struct-out negative-blame-struct)
         (struct-out contract-struct)
         contract-generate-function
         contract-interact
         contract-error
         generate-error
         value->contract
         impersonator-prop:contract
         has-impersonator-prop:contract?
         get-impersonator-prop:contract)

;;
;; require
;;

(require racket/string
         racket/list)

;;
;; data
;;

(struct blame-struct (path))
(struct positive-blame-struct blame-struct ())
(struct negative-blame-struct blame-struct ())
(struct contract-struct (name protect generate interact))

(define-values (impersonator-prop:contract
                has-impersonator-prop:contract?
                get-impersonator-prop:contract)
  (make-impersonator-property 'contract))

;;
;; functions
;;

(define (contract-generate-function ctc)
  (define generate (contract-struct-generate ctc))
  (if generate
      (generate)
      (generate-error (contract-struct-name ctc))))

(define (contract-interact val [n 1])
  (define ctc (value->contract val))
  (when ctc
    (for ([_ (in-range n)])
      ((contract-struct-interact ctc) val))))

(define CTC-FMT
  (string-join
   '("contract violation"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a")
   "\n  "))

(define (contract-error blm name val)
  (define error-msg
    (format CTC-FMT name val (blame-struct-path blm)))
  (raise-user-error error-msg))

(define GEN-FMT
  "cannot generate ~a")

(define ((generate-error name))
  (raise-user-error (format GEN-FMT name)))

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))
