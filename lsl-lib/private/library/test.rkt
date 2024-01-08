#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/id-table
                     syntax/parse
                     syntax/parse/class/struct-id)
         racket/class
         (except-in racket/contract blame?)
         racket/match
         racket/provide
         racket/string
         rackunit
         rackunit/text-ui
         "../syntax/expand.rkt"
         "../syntax/compile.rkt"
         "../contract/common.rkt"
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
   $check-random
   $check-range
   $check-satisfied
   $check-within
   $check-raises

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

(define-syntax ($check-random stx)
  (syntax-parse stx
    [(_ actual expected)
     (push-form!
      (syntax/loc stx
        (let ([rng-vec (make-rng-vec)])
          (check-equal? (with-rng-vec rng-vec actual)
                        (with-rng-vec rng-vec expected)))))]))

(define (make-rng-vec)
  (pseudo-random-generator->vector
   (make-pseudo-random-generator)))

(define-syntax-rule (with-rng-vec rng-vec e)
  (parameterize ([current-pseudo-random-generator
                  (vector->pseudo-random-generator rng-vec)])
    e))

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
      (syntax/loc stx (check-exn exn:user? (λ () body))))]
    [(_ body:expr exn:struct-id)
     (push-form!
      (syntax/loc stx
        (check-exn
         (λ (e)
           (and (exn:user? e)
                (exn.predicate-id (exn:user-value e))))
         (λ () body))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verification

(define FUEL 10)

(define (proxy->contract p)
  (and (proxy? p) (proxy-contract p)))

(define-syntax ($check-contract stx)
  (syntax-parse stx
    [(_ name:id (~optional n:number #:defaults ([n #'1])))
     #:with thk-body (syntax/loc stx (check-contract name 'name n))
     (push-form!
      #'(with-default-check-info*
          (list (make-check-params (list name (~? n))))
          (λ () thk-body)))]))

(define-check (check-contract val name n)
  (define (gen-mode ctc)
    (send ctc generate FUEL))
  (define ctc (proxy->contract val))
  (if ctc
      (for ([_ (in-range n)])
        (check-or-verify-contract ctc val name gen-mode))
      (fail-check (format "unknown contract for ~a" name))))

(define-syntax ($verify-contract stx)
  (syntax-parse stx
    [(_ name:id)
     #:with thk-body (syntax/loc stx (verify-contract name 'name))
     (push-form!
      #'(with-default-check-info*
          (list (make-check-params (list name)))
          (λ () thk-body)))]))

(define-check (verify-contract val name)
  (define (sym-mode ctc) (send ctc symbolic))
  (define ctc (proxy->contract val))
  (if ctc
      (check-or-verify-contract ctc val name sym-mode)
      (fail-check (format "unknown contract for ~a" name))))

(define (check-or-verify-contract ctc val name mode)
  (match (send ctc interact val name mode)
    [(list eg exn)
     (fail-check (format VERIFY-FMT eg (indent (exn-message exn))))]
    [(none)
     (fail-check "failed to generate values associated with contract")]
    [#f
     (void)]))

(define (indent str)
  (string-append "  " (string-replace str "\n" "\n    ")))

(define VERIFY-FMT
  (string-join
   '("discovered a counterexample"
     "counterexample: ~a"
     "error:"
     "~a")
   "\n  "))
