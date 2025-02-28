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
LSL tightly integrates contracts
and property-based randomized testing.

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
                       list list-ref list? memq memq? null null? remove rest reverse second seventh sixth third empty
		       member?)))
@(define ho (select '(identity andmap apply argmax argmin compose filter foldl foldr map memf ormap
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
    (posn-x a-posn)]
  }

@defform[(define-mutable-struct structure-name (field-name ...))]{
  Like @racket[define-struct], except functions that mutate the struct are available:
  @racketidfont{set-}@racket[structure-name]@racketidfont{-}@racket[field-name]@racketidfont{!} takes an
  instance of the structure and a value, and sets the field named by
  @racket[field-name] in the given instance of the structure to the given value.

  @examples[#:eval evaluator #:no-prompt #:label #f
    (define-mutable-struct box [value])
    (define a-box (make-box 0))
    (set-box-value! a-box 1)
    (box-value a-box)]
}

@defform[(begin expr0 ... expr)]{
  Executes the given expressions in sequence, returning the last @racket[expr].
}

@defform[(set! id expr)]{
  Mutates the variable @racket[id] with the value of @racket[expr].
}

@defform[(raise (make-struct-name ...))]{
  Raises an exception, where @racket[make-struct-name] constructs a struct
  that can have multiple arguments.
  @examples[#:eval evaluator #:label #f
  (define-struct foo ())
  (eval:error (raise (make-foo)))]
}

@defform*[[(check-raises expression)
          (check-raises expression structure-name)]]{
  Checks that the @racket[expression] raises an exception via @racket[raise], where
  the name of the struct raised in the exception matches @racket[structure-name], if it is present.
  @examples[#:eval evaluator #:label #f
  (define-struct foo ())
  (define (always-raise) (raise (make-foo)))
  (check-raises (always-raise))
  (check-raises (always-raise) foo)
  (define-struct bar ())
  (check-raises (always-raise) bar)]
}

@defproc[(remove-duplicates [l list?]) list?]{
  Removes duplicates from a list without altering its order using @racket[equal?] for equality.
  @examples[#:eval evaluator #:label #f
  (remove-duplicates '(0 1 1 2))]
}

@section{Testing}

@defform[(test-suite "name" ...)]{
Declares a named block of tests. Aside from printing and for autograding, no different from
putting @racket[check-expect] or similar at the top-level.
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
                 (generate (lambda (fuel) (* 2 (contract-generate Integer fuel))))
                 (feature "positive?" positive?)
                 (shrink (lambda (fuel x)
                           (let ([y (/ x 2)])
                             (if (even? y) y (sub1 y)))))))]

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

  @defsubform[(feature name-string feature-expr)]{
    The @racket[feature-expr] function takes one argument,
    a value satisfying the contract. It then computes some
    property of that value for use in Tyche.
    An @racket[Immediate] can have more than one @racket[feature].
  }

  @defsubform[(shrink shrink-expr)]{
    The @racket[shrink-expr] function takes two arguments,
    fuel and a value to shrink. The value to shrink is
    guaranteed to satisfy the contract.
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
    during property-based testing.
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
  @examples[#:eval evaluator #:label #f
    (define-struct my-point (x y))
    (contract-generate (Struct my-point [Integer Integer]))]
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

@deftogether[(@defidform[Any]
              @defidform[True]
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

@defform[(Maybe contract)]{
  Either @racket[#f] or a value satisfying the specified contract.

  To use a @racket[Maybe] value, you must check if the output is a Boolean with
  @racket[false?], or by using the contract for the associated type.

  @examples[#:eval evaluator #:label #f
    (: bigger-than-three (-> Integer (Maybe Integer)))
    (define (bigger-than-three n)
      (if (> n 3) n #f))

    (let ([x (bigger-than-three 1)])
      (if (false? x) "small" x))

    (filter integer? (map bigger-than-three (list 1 2 3 4 5)))
  ]
}


@defform[(-> arg-contract ... result-contract)]{
  A shorthand for a @racket[Function] contract
  with no dependencies.
}

@defform[(Record maybe-folding trace-id)
         #:grammar
         [(maybe-folder (code:line)
                        folding-expr)]]{
  @margin-note{
    The name of this contract rhymes with @emph{sword}
    since the contract @emph{records} values in a trace.
  }
  Uses the folding function @racket[folding-expr]
  to integrate values that pass through the contract
  into the value at @racket[trace-id].
  The folding function takes two arguments:
  the current value of @racket[trace-id]
  and the contracted value.
  @examples[#:eval evaluator #:label #f
    (: down-prev (Maybe Natural))
    (define down-prev #f)

    (: down-folder (-> (Maybe Natural) Natural Natural))
    (define (down-folder prev cur)
      (cond
        [(equal? prev cur) #f]
        [else cur]))

    (: down (-> (AllOf Natural (Record down-folder down-prev))))
    (define down
      (let ([x 3])
        (lambda ()
          (begin
            (set! x (if (< x 1) 0 (sub1 x)))
            x))))

    (down)
    (down)
    (down)
    (eval:error (down))]

  If no folding function is provided,
  values will be appended to the end of
  list at @racket[trace-id].
  @examples[#:eval evaluator #:label #f
    (define-contract Increasing
      (AllOf (List Real)
             (lambda (xs) (equal? (sort xs <) xs))))

    (: up-results Increasing)
    (define up-results '())

    (: up (-> (AllOf Natural (Record up-results))))
    (define up
      (let ([x 0])
        (lambda ()
          (begin
            (set! x (modulo (add1 x) 4))
            x))))

    (up)
    (up)
    (up)
    (eval:error (up))]
}

@section{Property-Based Randomized Testing}

@defform*[[(check-contract id)
           (check-contract id attempts)
           (check-contract id attempts size-expr)]]{
  Attempts to break the contract placed on @racket[id]
  by randomly generating inputs.
  The number of times it attempts to do so is
  controlled by @racket[attempts].
  The @racket[size-expr] controls the fuel given to
  generators. If @racket[size-expr] is a number,
  that is used as the fuel. If it is a procedure,
  then it accepts the iteration number and
  returns the fuel.
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

@defform[(with-traces body ...)]{
  After executing the @racket[body] expressions,
  @racket[with-traces] restores the value of traces
  to their values before the @racket[body] expressions ran.
  This is useful when you have a set of tests that use @racket[Record] on the same trace.
  @examples[#:eval evaluator #:label #f
    (define-contract AtMostOne
      (AllOf Natural (lambda (x) (<= x 1))))

    (: num-calls AtMostOne)
    (define num-calls 0)

    (: say-hi (-> (Record (lambda (old _) (add1 old)) num-calls)))
    (define (say-hi) "hi")

    (with-traces
      (say-hi))

    (with-traces
      (say-hi))

    (eval:error
      (with-traces
        (say-hi)
        (say-hi)))]
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

@defproc[(distinguishable? [thunk1 (-> any/c)] [thunk2 (-> any/c)]) boolean?]{
  Returns a Boolean that determines whether
  @racket[thunk1] and @racket[thunk2]
  are statistically distinguishable
  by how much time they take.
}

@defproc[(visualize [args list?] [f (-> any/c any/c)]) any/c]{
  To use this, @racket[(require lsl/performance)].

  Outputs a visualization graphing how much time
  @racket[f] takes for each of the given @racket[args].
}

@section{Concurrency}

@margin-note{
  Concurrency is not the same as parallelism.
  In parallelism,
  computations occur simultaneously,
  not just in an arbitrary order.
}
A concurrent system is one in which the order of computations occur may vary.
There are many models of concurrency, but the one supported by LSL is
based on the @emph{actor model}.

An actor system consists of a number of actors (or processes) that are
identified by a unique name. Actors send immutable values (or messages) to one
another. These messages are then processed by the receiving actor. Messages are
guaranteed to be delivered in FIFO (first-in-first-out) between two actors.
However, the order in which messages are delivered among all actors is
unspecified. When starting an actor system, a scheduler can be provided to fix
some delivery policy.

@defform[(Packet contract)]{
  Describes a packet sent from a process identified by a @racket[String], sent to
  a process identified by a @racket[String], and
  containing a message that satisfies @racket[contract].
}

@defproc[(packet-from [p (Packet Any)]) String]{
 Extracts an identifier representing the process (or actor) that a packet was sent from.
}

@defproc[(packet-to [sp (Packet Any)]) String]{
 Extracts an identifier representing the process (or actor) that a packet was sent to.
}

@defproc[(packet-msg [sp (Packet Any)]) Any]{
 Extracts the message (data) from a packet.
}

@defproc[(packet? [x Any]) Boolean]{
 Determines if its input is a packet.
}

@defform[(SendPacket contract)]{
  Describes a packet to send to a process identified by a @racket[String] and
  containing a message that satisfies @racket[contract].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: sp (SendPacket Natural))
    (define sp (send-packet "process-1" 42))]
}

@defproc[(send-packet [to string?] [msg any/c]) (SendPacket Any)]{
  A packet to be sent to process @racket[to] with message @racket[msg].
}

@defproc[(send-packet-to [sp (SendPacket Any)]) String]{
 Extracts an identifier representing the process (or actor) that a packet should be sent to.
}

@defproc[(send-packet-msg [sp (SendPacket Any)]) Any]{
 Extracts the message (data) from a packet to be sent.
}

@defproc[(send-packet? [x Any]) Boolean]{
 Determines if its input is a packet to be sent.
}

@defform[(ReceivePacket contract)]{
  Describes a packet received from a process identified by a @racket[String]
  containing a message that satisfies @racket[contract].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: rp (ReceivePacket Natural))
    (define rp (receive-packet "process-1" 42))]
}

@defproc[(receive-packet [from string?] [msg any/c]) (ReceivePacket Any)]{
  A packet that was sent from process @racket[from] with message @racket[msg].
}

@defproc[(receive-packet-from [rp (ReceivePacket Any)]) String]{
 Extracts an identifier representing the process (or actor) that a packet was sent from.
}

@defproc[(receive-packet-msg [rp (ReceivePacket Any)]) Any]{
 Extracts the message (data) from a received packet.
}

@defproc[(receive-packet? [x Any]) Boolean]{
 Determines if its input is a received packet.
}

@defform[(Action contract)]{
  Describes an action containing a new state
  that satisfies @racket[contract] and a list of packets to send satisfying
  @racket[(List (SendPacket Any))].
  @examples[#:eval evaluator #:no-prompt #:label #f
    (: a (Action Natural))
    (define a (action 5 (list (send-packet "process-1" 42)
                              (send-packet "process-1" 43)
                              (send-packet "process-2" "Hello!"))))]
}

@defproc[(action [state Any] [packets (List (SendPacket Any))]) (Action Any)]{
  The result of a process handler: combines the updated process state and a list of packets to
  send (which may be empty).
  The list of packets is sorted (per process) from earliest to latest.
  In the example above,
  @racket[process-1] is guaranteed to receive the @racket[42] packet
  before it receives the @racket[43] packet.
  However, the relative order the packets are received
  among different processes is unknown @emph{a priori}
  (as it is determined by the scheduler).
}

@defidform[Process]{
A contract for a process.
}

@defform/subs[#:id process
              #:literals
              (name on-start on-receive)
              (process clause ...)
              ([clause
                 (name s)
                 (on-start handler)
                 (on-receive handler)])]{
  Defines a process that represents a single actor in a concurrent system.
  All of the clauses of a @racket[process] description are mandatory.

  @defsubform[(name s)
              #:contracts
              ([s String])]{
    Names a process. This needs to be unique across all processes in a system.
  }

  @defsubform[(on-start handler)
              #:contracts
              ([handler (-> (List String) (Action contract))])]{
    The @racket[handler] function is applied to the list of all the other
    processes (as strings) when the process starts. The result of the call is an action,
    containing the new state of the process and a list of packets to send to other processes.
  }

  @defsubform[(on-receive handler)
              #:contracts
              ([handler (-> contract ReceivePacket (Action contract))])]{
    The @racket[handler] function is applied to the process's state and a packet
    when the process receives a new packet. The result of the call is an action,
    containing the new state of the process and a list of packets to send to other processes.
  }
}

@defproc[(start [scheduler (-> (List (Packet Any)) (Packet Any))]
                [processes (List Process)])
                (List (Tuple String Any))]{
  Runs a concurrent program using a fixed list of processes.
  When no more messages need to be processed, the function returns a list of pairs
  of the process name and final state.
}

@defproc[(start-debug [scheduler (-> (List (Packet Any)) (Packet Any))]
                      [processes (List Process)])
                      (List (Tuple String Any))]{
  Like @racket[start], but prints to the interactions window as each message is received
  and what the corresponding process state updates to.
}

@defproc[(start-gui [scheduler (-> (List (Packet Any)) (Packet Any))]
                    [processes (List Process)]
                    [summarize-state (-> Any Any)])
                    (List (Tuple String Any))]{
  Like @racket[start], but runs the program in an interactive graphical interface instead.
  The interface allows you to step forward and backward through the program, viewed through
  either a graph showing the current packet being sent/received, a table of the current messages
  waiting to be processed, or a history of all process states up to the current moment.

  The extra argument, @racket[summarize-state], is a function that is applied to the process
  state before it is displayed, since large process states can make the interface harder to use.
  The @racket[identity] function can be provided if you want the entire process state visible.
}
