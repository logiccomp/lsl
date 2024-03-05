#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/sequence
                     racket/string
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/struct)
         json
         (except-in net/http-easy
                    proxy?)
         racket/file
         racket/provide
         racket/runtime-path
         racket/struct
         racket/splicing
         (prefix-in ^ rosette/safe)
         syntax/parse/define
         version/utils
         "test.rkt"
         "../contract/common.rkt"
         "../syntax/grammar.rkt"
         "../syntax/expand.rkt"
         "../syntax/interface.rkt"
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 #%app
 #%top
 quote

 disable-contracts!
 enable-contracts!

 (filtered-out
  (strip "$")
  (combine-out
   $#%module-begin
   $#%datum
   $#%top-interaction

   $...
   $require
   $lambda
   $λ
   $begin
   $letrec
   $let
   $let*
   $else
   $cond
   $if
   $and
   $or
   $set!
   $raise
   $local

   $define-struct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interposition forms

(define-syntax $#%module-begin
  (syntax-parser
    [(_ form:expr ...)
     #'(#%module-begin
        (check-version)
        form ...
        (define-run-tests run-tests print-tests)
        (provide run-tests)
        (module+ main
          (print-tests)))]))

(define-syntax $#%datum
  (syntax-parser
    [(_ . (~or e:number e:boolean e:string e:character))
     #'(^#%datum . e)]))

(define-syntax $#%top-interaction
  (syntax-parser
    [(_ . e)
     #'(#%top-interaction . (with-vc-reset e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forms

(define-syntax $...
  (syntax-parser
    [_ (syntax/loc this-syntax
         (error '… "expected a finished expression, but found a template"))]))

(define-syntax-parse-rule ($require (~or* mod:string mod:id) ...)
  (^require mod ...))

(define-syntax-parse-rule ($lambda (param:id ...) body:expr)
  (^lambda (param ...) body))

(define-syntax-parse-rule ($λ (param:id ...) body:expr)
  (^λ (param ...) body))

(define-syntax-parse-rule ($begin body:expr ...+)
  (^begin body ...))

(define-syntax-parse-rule ($letrec ([var:id rhs:expr] ...) body:expr)
  (^letrec ([var rhs] ...) body))

(define-syntax-parse-rule ($let ([var:id rhs:expr] ...) body:expr)
  (^let ([var rhs] ...) body))

(define-syntax-parse-rule ($let* ([var:id rhs:expr] ...) body:expr)
  (^let* ([var rhs] ...) body))

(define-syntax ($else stx)
  (raise-syntax-error 'else "else must be used in a cond"))

(define-syntax $cond
  (syntax-parser
    #:literals ($else)
    [(_ [(~and (~not $else) guard) arm:expr] ...+ [$else final-arm:expr])
     #:declare guard (expr/c #'is-boolean? #:name "question")
     #'(^cond [guard.c arm] ... [^else final-arm])]
    [(_ [(~and (~not $else) guard) arm:expr] ...+)
     #:declare guard (expr/c #'is-boolean? #:name "question")
     #:with final-arm
     (syntax/loc this-syntax
       (error 'cond "all question results were false"))
     #'(^cond [guard.c arm] ... [^else final-arm])]))

(define-syntax-parse-rule ($if guard then:expr else:expr)
  #:declare guard (expr/c #'is-boolean? #:name "question")
  (^if guard.c then else))

(define-syntax-parse-rule ($and arg ...+)
  #:declare arg (expr/c #'is-boolean?)
  (^and arg.c ...))

(define-syntax-parse-rule ($or arg ...+)
  #:declare arg (expr/c #'is-boolean?)
  (^or arg.c ...))

(define-syntax-parse-rule ($set! x:id arg:expr)
  (^set! x arg))

(begin-for-syntax
  (define-syntax-class def
    #:literals (define-protected declare-contract)
    #:description "definition"
    (pattern (define-protected (~or x:id x:function-header) e:expr))
    (pattern (declare-contract x:id c:expr))))

(define-syntax-parse-rule ($local [d:def ...] e:expr)
  (^let () d ... e))

(define-syntax-rule ($raise v)
  (do-raise v))

(define (do-raise v)
  (define cur (current-allowed-exns))
  (when (and cur (ormap (λ (pred?) (pred? v)) cur))
    (^assume #f))
  (raise (exn:fail:lsl:user (format "exception raised: ~v" v)
                            (current-continuation-marks)
                            v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs

(begin-for-syntax
  (define (struct-contract-macro sname)
    (syntax-parser
      [(_ ctc ...)
       #`(Struct #,sname ctc ...)])))

(define-syntax $define-struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #:with Name (kebab->camel #'name)
     #:with (k ...)
     (for/list ([k (in-naturals)] [field-id (in-syntax #'(field ...))])
       #`(#%datum . #,k))
     #:with (_ ctor pred acc ...)
     (build-struct-names #'name (syntax-e #'(field ...)) #f #t)
     #:with (_ _ _ mut ...)
     (build-struct-names #'name (syntax-e #'(field ...)) #t #f)
     #'(begin
         (define-syntax Name
           (contract-macro
            (struct-contract-macro #'name)))
         (^struct name root (field ...)
                  #:transparent
                  #:mutable
                  #:constructor-name ctor
                  #:methods gen:equatable
                  [(define (base-equal? self other)
                     (and (pred self)
                          (pred other)
                          (equal? (acc self) (acc other)) ...))]
                  #:property prop:custom-print-quotable 'never
                  #:methods gen:custom-write
                  [(define write-proc
                     (make-constructor-style-printer
                      (λ (obj) 'ctor)
                      (λ (obj) (list (acc obj) ...))))])
         (set! pred (redirect-pred pred))
         (set! acc (redirect-accessor 'acc pred acc k)) ...
         (set! mut (redirect-mutator 'acc pred mut k)) ...)]))

(define (redirect-pred pred)
  (procedure-rename
   (λ (val)
     (^for/all ([val val])
       (pred (unproxy val))))
   (object-name pred)))

(define (redirect-accessor name pred acc k)
  (procedure-rename
   (λ (val)
     (^for/all ([val val])
       (check-struct-type name val pred)
       (let go ([val val])
         (if (proxy? val)
             ((vector-ref (proxy-info val) k)
              (go (proxy-target val)))
             (acc val)))))
   (object-name acc)))

(define (redirect-mutator name pred mut k)
  (procedure-rename
   (λ (val to-set)
     (^for/all ([val val])
       (check-struct-type name val pred)
       (let go ([val val] [to-set to-set])
         (if (proxy? val)
             (go (proxy-target val)
                 ((vector-ref (proxy-info val) k) to-set))
             (mut val to-set)))))
   (object-name mut)))

(define (check-struct-type name val pred)
  (unless (pred (unproxy val))
    (raise-argument-error name (format "~a" (object-name pred)) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; versioning

(define-runtime-path VERSION-PATH "../../version.json")
(define VERSION-URL "https://raw.githubusercontent.com/logiccomp/lsl/main/lsl-lib/version.json")
(define VERSION-FMT "A new version (~a) of LSL is available. Please upgrade and restart DrRacket.\n")
(define CACHE-EXPIRE (* 60 60))

(define cache-path (build-path (find-system-path 'cache-dir) "lsl.rktd"))
(define current-version
  (with-input-from-file VERSION-PATH
    (λ () (hash-ref (read-json) 'version))))

(define (check-version)
  (with-handlers ([exn:fail? void])
    (define last-checked (hash-ref (get-version-cache) 'timestamp))
    (define check-delta (- (current-seconds) last-checked))
    (when (>= check-delta CACHE-EXPIRE)
      (update-cache))
    (define latest-version (hash-ref (get-version-cache) 'version))
    (define ready? (hash-ref (get-version-cache) 'ready))
    (when (and ready?
               (valid-version? current-version)
               (valid-version? latest-version)
               (version<? current-version latest-version))
      (printf VERSION-FMT latest-version))))

(define (update-cache)
  (put-version-cache
   (hash-set (get-version-cache) 'timestamp (current-seconds)))
  (define res (get VERSION-URL))
  (when (= (response-status-code res) 200)
    (put-version-cache
     (hash-set* (get-version-cache)
                'version (hash-ref (response-json res) 'version)
                'ready (hash-ref (response-json res) 'ready))))
  (response-close! res))

(define (get-version-cache)
  (if (file-exists? cache-path)
      (file->value cache-path)
      (hash 'timestamp 0
            'version current-version
            'ready #f)))

(define (put-version-cache val)
  (unless (file-exists? cache-path)
    (make-parent-directory* cache-path))
  (write-to-file val cache-path #:exists 'replace))
