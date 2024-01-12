#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/sequence
                     racket/string
                     syntax/parse
                     syntax/struct
                     threading)
         json
         (except-in net/http-easy
                    proxy?)
         racket/file
         racket/local
         racket/provide
         racket/runtime-path
         (prefix-in ^ rosette/safe)
         syntax/parse/define
         version/utils
         "test.rkt"
         "../contract/common.rkt"
         "../syntax/grammar.rkt"
         "../syntax/expand.rkt"
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 #%app
 #%top
 #%top-interaction
 quote
 local

 (filtered-out
  (strip "$")
  (combine-out
   $#%module-begin
   $#%datum

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forms

(define-syntax $...
  (syntax-parser
    [_ (syntax/loc this-syntax
         (error '(... ...) "expected a finished expression, but found a template"))]))

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
    [(_ [(~and (~not $else) guard:expr) arm:expr] ...+ [$else final-arm:expr])
     #'(^cond [guard arm] ... [^else final-arm])]
    [(_ [(~and (~not $else) guard:expr) arm:expr] ...+)
     #:with final-arm
     (syntax/loc this-syntax
       (error 'cond "all question results were false"))
     #'(^cond [guard arm] ... [^else final-arm])]))

(define-syntax-parse-rule ($if guard:expr then:expr else:expr)
  (^if guard then else))

(define-syntax-parse-rule ($and arg0:expr arg:expr ...+)
  (^and arg0 arg ...))

(define-syntax-parse-rule ($or arg0:expr arg:expr ...+)
  (^or arg0 arg ...))

(define-syntax-parse-rule ($set! x:id arg:expr)
  (^set! x arg))

(define ($raise v)
  (raise (exn:user (format "exception raised: ~v" v)
                   (current-continuation-marks)
                   (^vc) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs

(begin-for-syntax
  (define (struct-name->contract-name stx)
    (~> stx
        syntax-e
        symbol->string
        (string-replace "-" " ")
        string-titlecase
        (string-replace " " "")
        string->symbol
        (datum->syntax stx _)))

  (define (struct-contract-macro sname)
    (syntax-parser
      [(_ ctc ...)
       #`(Struct #,sname ctc ...)])))

(define-syntax $define-struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #:with Name (struct-name->contract-name #'name)
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
                  #:constructor-name ctor)
         (set! pred (redirect-pred pred))
         (set! acc (redirect-accessor pred acc k)) ...
         (set! mut (redirect-mutator pred mut k)) ...)]))

(define ((redirect-pred pred) val)
  (pred (unproxy val)))

(define (redirect-accessor pred acc k)
  (define (recur val)
    (if (proxy? val)
        ((vector-ref (proxy-info val) k)
         (recur (proxy-target val)))
        (acc val)))
  recur)

(define (redirect-mutator pred mut k)
  (define (recur val to-set)
    (if (proxy? val)
        (recur (proxy-target val)
               ((vector-ref (proxy-info val) k) to-set))
        (mut val to-set)))
  recur)

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
  (define latest-version
    (when (= (response-status-code res) 200)
      (hash-ref (response-json res) 'version)))
  (response-close! res)
  (put-version-cache
   (hash-set (get-version-cache) 'version latest-version)))

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
