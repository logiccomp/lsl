#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[racket/require]
@require[@for-label[(except-in racket/contract ->)
                    "label.rkt"]
         racket/list
         racket/sandbox
	 scribble/html-properties
	 scribble/core
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; utils

@(define evaluator (make-base-eval #:lang 'lsl))

@(define (select ids)
   (let* ([ids (sort ids symbol<?)]
          [ids (map (λ (id) (racket #,#`#,id)) ids)])
     (apply elem (add-between ids ", "))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Logical Student Language}
@author{Cameron Moy}
@author{Daniel Patterson}

@defmodule[lsl #:lang]

@section{Inherited from ISL}

@(define stx (select '(... lambda λ local letrec let* let define define-struct quote cond else if and or require)))
@(define test (select '(check-expect check-random check-satisfied check-within check-error check-member-of check-range)))
@(define eqn (select '(eq? equal?)))
@(define bool (select '(boolean=? boolean? not true false)))
@(define nums (select '(* + / = - < <= > >= abs add1 ceiling even? exact->inexact floor inexact->exact integer? max
                        min modulo negative? number? odd? pi positive? quotient real? remainder sgn sub1 sqr zero?
			sqrt expt random)))
@(define str (select '(string=? format string-length string-append list->string make-string string string->list
                       string->number string->symbol string-contains? string-copy string-downcase string-ref
                       string-upcase string? substring)))
@(define lst (select '(append assoc assq build-list car cdr cons cons? eighth empty? fifth first fourth length
                       list list-ref list? memq null null? remove rest reverse second seventh sixth third empty)))
@(define ho (select '(identity andmap apply argmax argmin compose filter foldl foldr for-each map memf ormap
                      procedure? sort)))

@tabular[#:style (style #f (list (attributes '((class . "boxed") (style . "border-spacing: 10px 5px")))))
         #:column-properties '(top)
(list (list @elem{Syntax} @stx)
      (list @elem{Testing} @test)
      (list @elem{Equality} @eqn)
      (list @elem{Booleans} @bool)
      (list @elem{Numbers} @nums)
      (list @elem{Strings} @str)
      (list @elem{Lists} @lst)
      (list @elem{Functions} @ho))]

@section{Contracts}

@defform[(Flat)]{
  @defsubform[(domain)]{ TODO }
  @defsubform[(check)]{ TODO }
  @defsubform[(generate)]{ TODO }
  @defsubform[(symbolic)]{ TODO }
}
@defform[(List)]{ TODO }
@defform[(Function)]{ TODO }
@defform[(OneOf)]{ TODO }
@defform[(And)]{ TODO }
@defform[(Struct)]{ TODO }

@defidform[Boolean]{ TODO }
@defform[(Constant)]{ TODO }
@defidform[True]{ TODO }
@defidform[Integer]{ TODO }
@defidform[Real]{ TODO }
@defidform[Natural]{ TODO }
@defidform[String]{ TODO }
@defidform[Symbol]{ TODO }
@defform[(Record)]{ TODO }
@defform[(->)]{ TODO }

@defform[(:)]{ TODO }
@defform[(define-contract)]{ TODO }
@defproc[(contract-predicate) any/c]{ TODO }

@section{Property-Based Testing}

@defform[(check-contract)]{ TODO }
@defproc[(contract-generate) any/c]{ TODO }

@section{Verification}

@defform[(verify-contract)]{ TODO }
@defproc[(contract-symbolic) any/c]{ TODO }

@section{State Machines}

@defform[(state-machine)]{ TODO }
@defproc[(accepting?) any/c]{ TODO }
@defproc[(accepts?) any/c]{ TODO }
@defproc[(next) any/c]{ TODO }

@defform[(regular-expression)]{
  @defsubform[(complement)]{ TODO }
  @defsubform[(seq)]{ TODO }
  @defsubform[(seq-prefix)]{ TODO }
  @defsubform[(union)]{ TODO }
  @defsubform[(star)]{ TODO }
  @defsubform[(epsilon)]{ TODO }
}

@section{Performance}

@defproc[(visualize) any/c]{ TODO }
@defproc[(ticks) any/c]{ TODO }
