#lang racket/base

;;
;; provide
;;

(provide (struct-out blame-struct)
         (struct-out positive-blame-struct)
         (struct-out negative-blame-struct)
         (struct-out flat-contract-struct)
         (struct-out contract-struct)
         contract-generate-function
         contract-exercise
         contract-verify
         contract-error
         verify-error
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
(struct contract-struct (name protect generate symbolic interact))
(struct flat-contract-struct contract-struct (predicate))

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

(define (contract-exercise val [n 1])
  (define ctc (value->contract val))
  (define mode contract-generate-function)
  (when ctc
    (for ([_ (in-range n)])
      ((contract-struct-interact ctc) mode val))))

(define (contract-symbolic-function ctc)
  (define generate (contract-struct-symbolic ctc))
  (define msg (format "symbolic ~a" (contract-struct-name ctc)))
  (if generate (generate) (generate-error msg)))

(define (contract-verify val)
  (define ctc (value->contract val))
  (define mode contract-symbolic-function)
  (when ctc ((contract-struct-interact ctc) mode val)))

(define VERIFY-FMT
  (string-join
   '("verification failure"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a")
   "\n  "))

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

(define (verify-error blm name val)
  (define error-msg
    (format VERIFY-FMT name val (blame-struct-path blm)))
  (raise-user-error error-msg))

(define GEN-FMT
  "cannot generate ~a")

(define ((generate-error name))
  (raise-user-error (format GEN-FMT name)))

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))
