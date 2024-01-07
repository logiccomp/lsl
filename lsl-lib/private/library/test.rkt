#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse
                     syntax/parse/class/struct-id)
         racket/class
         racket/contract
         racket/provide
         racket/string
         rackunit
         rackunit/text-ui
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (filtered-out
  (strip "$")
  (combine-out
   $run-tests
   $with-tests

   $check-error
   $check-expect
   $check-member-of
   ;; TODO: $check-random
   $check-range
   $check-satisfied
   $check-within

   $check-contract
   $verify-contract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test environments

(begin-for-syntax
  (define expect-forms null)
  (define dont-push? (make-parameter #f))

  (define (push-form! stx)
    (define stx* #`(test-begin #,stx))
    (if (dont-push?)
        stx*
        (match (syntax-local-context)
          [(or 'module 'module-begin)
           (set! expect-forms (cons stx* expect-forms))
           #'(void)]
          [(or 'top-level 'expression (? list?))
           #`(void
              (run-tests
               (test-suite
                "top-level tests"
                #,stx*)))]))))

(define-syntax $with-tests
  (syntax-parser
    [(_ e:expr ...)
     (parameterize ([dont-push? #t])
       (local-expand
        #'(let ([result (void)])
            (run-tests
             (test-suite
              "local tests"
              (set! result (let () e ...))))
            result)
        (syntax-local-context)
        null))]))


(define-syntax $run-tests
  (syntax-parser
    [(_)
     #:with (form ...) expect-forms
     (begin0
       (if (empty? expect-forms)
           #'(void)
           #'(void
              (run-tests
               (test-suite
                "module-level tests"
                form ...))))
       (set! expect-forms null))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checks

(define-syntax ($check-expect stx)
  (syntax-parse stx
    [(_ actual expected)
     (push-form!
      (syntax/loc stx (check-equal? actual expected)))]))

(define-syntax ($check-within stx)
  (syntax-parse stx
    [(_ actual expected ϵ)
     #:declare actual (expr/c #'number?)
     #:declare expected (expr/c #'number?)
     #:declare ϵ (expr/c #'number?)
     (push-form!
      (syntax/loc stx
        (let ([a actual.c] [e expected.c] [v ϵ.c])
          (check-within a e v))))]))

(define-syntax ($check-member-of stx)
  (syntax-parse stx
    [(_ actual expecteds ...)
     (push-form!
      (syntax/loc stx (check-true ($member actual (list expecteds ...)))))]))

(define-syntax ($check-range stx)
  (syntax-parse stx
    [(_ actual low high)
     #:declare low (expr/c #'number?)
     #:declare high (expr/c #'number?)
     #:declare actual (expr/c #'number?)
     (push-form!
      (syntax/loc stx
        (let ([l low.c] [a actual.c] [h high.c])
          (check-true (<= l a h)))))]))

(define-syntax ($check-satisfied stx)
  (syntax-parse stx
    [(_ actual pred)
     #:declare pred (expr/c #'predicate/c)
     (push-form!
      (syntax/loc stx
        (let ([v pred.c])
          (check-pred v actual))))]))

(define-syntax ($check-error stx)
  (syntax-parse stx
    [(_ body:expr)
     (push-form!
      (syntax/loc stx (check-exn (λ _ #t) (λ () body))))]
    [(_ body:expr msg)
     #:declare msg (expr/c #'string?)
     (push-form!
      (syntax/loc stx
        (let ([v msg.c])
          (check-exn (matches? v) (λ () body)))))]))

(define ((matches? msg) e)
  (string-contains? (exn-message e) msg))

(define-syntax ($check-raises stx)
  (syntax-parse stx
    [(_ body:expr)
     (push-form!
      (syntax/loc stx (check-exn exn:fail:user? (λ () body))))]
    [(_ body:expr exn:struct-id)
     (push-form!
      (syntax/loc stx
        (check-exn
         (λ (e)
           (and (exn:fail:user? e)
                (exn.predicate-id (exn:fail:user-value e))))
         (λ () body))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verification

(define FUEL 10)

(define-syntax ($check-contract stx)
  (syntax-parse stx
    [(_ val:id (~optional n:number #:defaults ([n #'1])))
     (push-form!
      (syntax/loc stx
        (check-contract val 'val n)))]))

(define-check (check-contract val name n)
  (define (gen-mode ctc) (send ctc generate FUEL))
  (for ([_ (in-range n)])
    (check-or-verify-contract val name gen-mode)))

(define-syntax ($verify-contract stx)
  (syntax-parse stx
    [(_ val:id)
     (push-form!
      (syntax/loc stx
        (verify-contract val 'val)))]))

(define-check (verify-contract val name)
  (define (sym-mode ctc) (send ctc symbolic))
  (check-or-verify-contract val name sym-mode))

(define (check-or-verify-contract val name mode)
  (define ctc (and (proxy? val) (proxy-contract val)))
  (when ctc
    (define countereg (send ctc interact val name mode))
    (unless (none? countereg)
      (error name VERIFY-FMT countereg))))

(define VERIFY-FMT
  (string-join
   '("verification failure"
     "counterexample: ~a")
   "\n  "))
