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

The Logical Student Language (LSL)
is a teaching language that extends the
Intermediate Student Language (ISL)
from @emph{How to Design Programs}
with features that support formally reasoning about programs.
In particular,
LSL tightly integrates contracts,
property-based randomized testing,
and symbolic execution.

@section{Inherited from ISL}

@(define stx (select '(... lambda λ local letrec let* let define quote cond else if and or require)))
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
                       list list-ref list? memq null null? remove rest reverse second seventh sixth third empty
		       member?)))
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

@section{Extended from ISL}
@defform[(define-struct structure-name (field-name ...))]{

  Defines a new structure called @racket[structure-name]. The structure's fields are
  named by the @racket[field-name]s. After the @racket[define-struct], the following new
  functions are available:

  @itemize[

    @item{@racketidfont{make-}@racket[structure-name] : takes a number of
          arguments equal to the number of fields in the structure,
          and creates a new instance of that structure.}

    @item{@racket[structure-name]@racketidfont{-}@racket[field-name] : takes an
          instance of the structure and returns the value in the field named by
          @racket[field-name].}
    @item{@racketidfont{set-}@racket[structure-name]@racketidfont{-}@racket[field-name]@racketidfont{!} : takes an
          instance of the structure and a value, and sets the field named by
          @racket[field-name] in the given instance of the structure to the given value.}

    @item{@racket[structure-name]@racketidfont{?} : takes any value, and returns
          @racket[#t] if the value is an instance of the structure.}
  ]

  The name of the new functions introduced by @racket[define-struct]
  must not be the same as that of other functions or variables,
  otherwise @racket[define-struct] reports an error.

  @examples[#:eval evaluator #:no-prompt #:label #f
    (define-struct posn [x y])
    (define a-posn (make-posn 0 0))
    (set-posn-x! a-posn 1)
    (posn-x a-posn)]
  }

@section{Contracts}

@defform[(define-contract id contract)]{
  Creates @racket[id] as an alias for @racket[contract].

  @examples[#:eval evaluator #:no-prompt #:label #f
    (define-contract IntToInt
      (-> Integer Integer))]
}

@defform[(: id contract)]{
  Annotates @racket[id] with a contract.
  When @racket[id] is created with @racket[define],
  the contract is attached to the defined value.

  @examples[#:eval evaluator #:no-prompt #:label #f
    (: on-two-and-three (-> (-> Integer Integer) Integer))
    (define (on-two-and-three g)
      (+ (g 2) (g 3)))]

  Uses of @racket[on-two-and-three] are protected:

  @examples[#:eval evaluator #:label #f
    (on-two-and-three (lambda (x) (* x x)))
    (eval:error (on-two-and-three (lambda (x) (/ x 2))))]
}

@subsection{Primitives}

@defform[(Immediate clause ...)]{
  An @emph{immediate contract} is one that can be checked immediately,
  without imposing any future checks.
  As a shorthand,
  an ordinary LSL expression @racket[e]
  in a contract position
  is automatically converted to the immediate contract
  @racket[(Immediate (check e))].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (define-contract Even
      (Immediate (check (lambda (x) (and (integer? x) (even? x))))
                 (generate (lambda (fuel) (* 2 (random (add1 fuel)))))
                 (shrink (lambda (fuel x)
	                   (let ([y (/ x 2)])
                             (if (even? y) y (sub1 y)))))
                 (symbolic (lambda () (* 2 (contract-symbolic Integer))))))]

  @defsubform[(check predicate-expr)]{
    The @racket[predicate-expr] is expected to produce a predicate
    that determines if a value satisfies the contract.
  }

  @defsubform[(generate gen-expr)]{
    The @racket[gen-expr] should evaluate to a function
    that takes a single natural number,
    the @emph{fuel},
    and produces a value that satisfies the contract.
    Fuel provides a rough measure of how
    hard the generator is willing to work
    to produce a value.
  }

  @defsubform[(shrink shrink-expr)]{
    The @racket[shrink-expr] function takes two arguments,
    fuel and a value to shrink. The value to shrink is
    guaranteed to satisfy the contract.
  }

  @defsubform[(symbolic sym-expr)]{
    The @racket[sym-expr] function takes no arguments
    and produces a symbolic value that represents
    @emph{all} values that satisfy the contract.
  }
}

@defform[(Function clause ...)]{
  A function contract protects a function by constraining its
  inputs and outputs. Arguments are labeled so that dependent
  properties can be checked.
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: double (Function (arguments [x Integer])
                        (result (lambda (y) (= x y)))))
    (define (double x)
      (* 2 x))]

  The @racket[double] function violates this contract:
  @examples[#:eval evaluator #:label #f
    (double 0)
    (eval:error (double 1))]

  @defsubform[(arguments [id contract] ...)]{
    Describes the contracts on arguments.
    Any @racket[id] can be used in any @racket[contract]
    and will be bound to the concrete argument when the
    function is applied,
    so long as there is no cyclic dependency.
  }

  @defsubform[(result contract)]{
    Describes the result contract.
    All @racket[id] values are available in @racket[contract].
  }

  @defsubform[(raises exn-id)]{
    Permits the @racket[exn-id] struct to be raised in the function.
    Such exceptions are not considered failures
    during property-based testing and symbolic execution.
  }
}

@defform[(List contract)]{
  Describes a list of arbitrary length
  whose elements satisfy @racket[contract].
  @examples[#:eval evaluator #:label #f
    (contract-generate (List Integer))]
}

@defform[(Tuple contract ...)]{
  Describes lists where the k-th element
  satisfies the k-th @racket[contract].
  The list must have exactly the same number of elements
  as there are contracts.
  @examples[#:eval evaluator #:label #f
    (contract-generate (Tuple Boolean Integer))]
}

@defform[(OneOf contract ...)]{
  A value satisfying a @racket[OneOf] contract
  must satisfy @emph{exactly one} of the
  given @racket[contract]s.
  @examples[#:eval evaluator #:label #f
    (contract-generate (List (OneOf Boolean Integer)))]
}

@defform[(AllOf contract ...)]{
  A value satisfying an @racket[AllOf] contract
  must satisfy @emph{all} of the
  given @racket[contract]s.
  @examples[#:eval evaluator #:label #f
    (define-contract UnitInterval
      (AllOf Real
             (lambda (x) (< 0 x))
	     (lambda (x) (< x 1))))
    (: reciprocal (-> UnitInterval Real))
    (define (reciprocal x) (/ 1 x))
    (reciprocal 1/2)
    (eval:error (reciprocal -2))
    (eval:error (reciprocal 2))]
}

@defform[(Struct struct-id (field-contract ...))]{
  Struct contracts describe each individual field on the struct.
  Each struct definition automatically generates a shorthand
  for such a contract, as such:
  @examples[#:eval evaluator #:label #f
    (define-struct my-point (x y))
    (contract-generate (MyPoint Integer Integer))]
}

@defform[(All (id ...) contract)]{
  A universal contract that guarantees @racket[id]
  is used uniformally within @racket[contract].
  This form is often used to define
  parametrically polymorphic functions.
  @examples[#:eval evaluator #:label #f
    (: id (All (A) (-> A A)))
    (define (id x) x)
    (id 10)]
}

@defform[(Exists (id ...) contract)]{
  An existential contract that guarantees @racket[id]
  is used uniformally within @racket[contract].
  This form is often used for data abstraction.
  @examples[#:eval evaluator #:label #f
    (: counter-pkg (Exists (A) (Tuple (-> A) (-> A A) (-> A Integer))))
    (define counter-pkg
      (list (λ () 0)
            (λ (x) (+ x 1))
            (λ (x) x)))
    (define make-counter (first counter-pkg))
    (define counter-incr (second counter-pkg))
    (define counter-get (third counter-pkg))
    (counter-get (counter-incr (make-counter)))]
}

@subsection{Derived}

@deftogether[(@defidform[True]
              @defidform[False]
              @defidform[Boolean]
              @defidform[Natural]
              @defidform[Integer]
              @defidform[Real]
              @defidform[String]
              @defidform[Symbol])]{
  Atomic contracts that recognize their respective types.
}

@defform[(Constant expr)]{
  Only values @racket[equal?] to @racket[expr] are permitted.
}

@defform[(-> arg-contract ... result-contract)]{
  A shorthand for a @racket[Function] contract
  with no dependencies.
}

@defform[(Record maybe-folding trace-id)
         #:grammar
         [(maybe-folder (code:line)
                        folding-expr)]]{
  Uses the folding function @racket[folding-expr]
  to integrate values that pass through the contract
  into the value at @racket[trace-id].
  If no folding function is provided,
  values will be appended to the end of
  list at @racket[trace-id].
}

@section{Property-Based Randomized Testing}

@defform[(check-contract id maybe-attempts)
         #:grammar
         [(maybe-attempts (code:line)
                          nat-expr)]]{
  Attempts to break the contract placed on @racket[id]
  by randomly generating inputs.
  The number of times it attempts to do so is
  controlled by @racket[maybe-attempts].
  @examples[#:eval evaluator #:label #f
    (: bigger-even (-> Integer Even))
    (define (bigger-even x)
      (* 3 x))
    (check-contract bigger-even 20)]
}

@defform[(contract-generate contract maybe-fuel)
         #:grammar
         [(maybe-attempts (code:line)
                          nat-expr)]]{
  Generates a value that satisfies the given contract
  using the supplied fuel.
  @examples[#:eval evaluator #:label #f
    (contract-generate Even)]
}

@section{Verification via Symbolic Execution}

@defform[(verify-contract id)]{
  Similar to @racket[check-contract],
  except it uses symbolic execution
  try and break the contract.
  @examples[#:eval evaluator #:label #f
    (: usually-fine (-> Integer Even))
    (define (usually-fine x)
      (if (= x 1000) 17 (* 2 x)))
    (verify-contract usually-fine)]
}

@defform[(contract-symbolic contract)]{
  Returns a symbolic value that represents
  all values satisfying the given contract.
  @examples[#:eval evaluator #:label #f
    (contract-symbolic Even)]
}

@section{State Machines}

@defproc[(machine? [x any/c]) boolean?]{
  Identifies machines.
}

@defproc[(machine-accepting? [x any/c]) boolean?]{
  Identifies accepting machines.
}

@defproc[(machine-accepts? [m machine?] [l list?]) boolean?]{
  Returns @racket[#t] if @racket[m] ends in an accepting state after consuming every element of @racket[l].
}

@defproc[(machine-next [m machine?] [x any/c]) machine?]{
  Produces a machine starting from @racket[m] and consuming @racket[x].
}

@subsection{DFAs}

@defform[(dfa start
              (end ...)
              [state ([in next-state] ...)]
	      ...)]{
 Returns a deterministic finite automaton (DFA)
 that starts in state @racket[start]
 and where each state behaves as specified in the rules.

 @examples[#:eval evaluator #:label #f
   (define M
     (dfa s1 (s1)
          [s1 ([0 s2]
               [2 s1])]
          [s2 ([0 s1]
               [2 s2])]))
   (machine-accepts? M (list 2 0 2 0 2))
   (machine-accepts? M (list 0 2 0 2 0))]
}

@subsection{Regular Expressions}

@defform[(re re-form)]{
  Returns a machine that recognizes the given regular expression.

  @examples[#:eval evaluator #:label #f
    (define R (re (star (seq 'a 'b))))
    (machine-accepts? R (list 'a 'b 'a 'b))
    (machine-accepts? R (list 'a 'b 'a))]

  @defsubidform[literal]{
    A literal value recognizes itself.
  }

  @defsubform[(complement re)]{
    Recognizes anything that is not recognized not @racket[re].
  }

  @defsubform[(seq re ...)]{
    Recognizes a sequence of regular expressions.
  }

  @defsubform[(seq-prefix re ...)]{
    Recognizes prefixes of the given regular expression.
    In other words, @racket[(seq-prefix x y z)]
    is the same as
    @racket[(union (epsilon) x (seq x y) (seq x y z))].
  }

  @defsubform[(union re ...)]{
    Recognizes any of the given @racket[re].
  }

  @defsubform[(star re)]{
    Recognizes zero or more repetitions of @racket[re].
  }

  @defsubform[(epsilon)]{
    Recognizes the empty list.
  }
}

@section{Resources}

@defproc[(ticks [thunk (-> any/c)]) integer?]{
  Returns a unitless quantity that represents
  how much "time" it takes to execute @racket[thunk].
}

@defproc[(visualize [args list?] [f (-> any/c any/c)]) any/c]{
  Outputs a visualization graphing how much time
  @racket[f] takes for each of the given @racket[args].
}

@section{Concurrency}

@defform[(SendPacket contract)]{
  Describes a packet to send to a process identified by a @racket[String] and
  containing a message that satisfies @racket[contract].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: SP Any)
    ;; (: SP (SendPacket Natural))
    (define SP (send-packet "process-1" 42))]
}

@defform[(ReceivedPacket contract)]{
  Describes a packet received from a process identified by a @racket[String]
  containing a message that satisfies @racket[contract].
  @examples[#:eval evaluator #:no-prompt #:label #f
    ;; (: RP (ReceivedPacket Natural))
    (: RP Any)
    (define RP (receive-packet "process-1" 42))]
}

@defform[(Action contract)]{
  Describes an action containing a new state
  that satisfies @racket[contract] and a list of packets to send satisfying @racket[(List (SendPacket Any))].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: A (Action Natural))
    (define A (action 5 (list (send-packet "process-1" 42) 
                              (send-packet "process-2" "Hello!"))))]
}

@defform/subs[#:id process
              #:literals
              (name on-start on-receive)
              (process clause ...)
              ([clause
                 (name s)
                 (on-start handler)
                 (on-receive handler)])]{Defines a process.}

All of the clauses of a @racket[process] description are mandatory:
@itemize[

@item{
@defform[(name s)
         #:contracts
         ([s String])]{
 gives a name to the process. This needs to
 be unique across all processes in a program.
 } 
}

@item{
@defform[(on-start handler)
         #:contracts
         ([handler (-> (List String) (Action ...))])]{
  tells LSL to call the function @racket[handler] with the list of all the other
  processes (string identifiers) when the process starts. The result of the call is an action,
  containing the new state of the process and a list of packets to send. LSL uses the resulting
  action and updates the state of the process and performs the sending of the packets to other processes.
 } 
}

@item{
@defform[(on-receive handler)
         #:contracts
         ([handler (-> Any ReceievePacket (Action ...))])]{
  tells LSL to call the function @racket[handler] with the process's state and the received packet
  when the process receives a packet. The result of the call is an action,
  containing the new state of the process and a list of packets to send. LSL uses the resulting
  action and updates the state of the process and performs the sending of the packets to other processes.
 } 
}
]