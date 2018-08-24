#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          behavior/fsm
          behavior/markov-chain
          (for-label behavior/fsm
                     behavior/markov-chain
                     racket/contract))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/function
                                racket/logging
                                racket/string
                                behavior/fsm
                                behavior/markov-chain)))

@;{============================================================================}

@title[#:version "1.0"]{Behavioral Models}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This package provides a number of modules that provide behavioral models. These
models, state machines, petri nets, Markov chains, etc. can be used to analyze
or simulate real-world systems or act as a test surrogate for components in
complex systems as they are developed.

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[]{Module behavior/fsm}
@defmodule[behavior/fsm]

This module provides the ability to define, and then execute, state machines
using a flexible yet capable model. The definition model provides a @italic{state
machine} that contains a set of discrete @italic{states}, each state has a set
of outbound @italic{transitions} each of which may be associated with a
@italic{trigger event}. Note, wherever possible the terminology and relationships
in the state machine  model follow those of the
@hyperlink["https://en.wikipedia.org/wiki/UML_state_machine"]{Unified Modeling
Language (UML)}.


State machine definition details:

@itemlist[
  @item{All state names, and events, are represented as symbols.}
  @item{Behaviors are represented as simple procedures that take the current
        @racket[machine-execution] as a parameter and return void. As the
        execution instance is immutable the behavior cannot affect the
        current state.}
  @item{Transition guards are represented as simple predicates that take the
        current @racket[machine-execution] as a parameter and return a boolean
        which indicates whether the transition is @italic{enabled}. As the
        execution instance is immutable the behavior cannot affect the
        current state.}
  @item{Additional events may be passed into the model, these will be recorded
        if received and will be ignored without error. See execution details
        below for event and transition behavior.}
  @item{The model supports @italic{internal transitions} that transition from
        one state to itself without triggering exit or event behavior(s).}
  @item{Reporters...}
  @item{Model validation rules:
        @itemlist[
          @item{The model must have one, and only one, start state.}
          @item{The model must have one, and only one, final state.}
          @item{Transitions marked as internal must have the same source and
                target state.}
        ]}
  @item{Limitations (compared to UML for example):
        @itemlist[
          @item{The model does not currently support nested, or hierarchical,
                states.}
          @item{The model does not currently support orthogonal regions.}
        ]}
]

@examples[ #:eval example-eval
(define simple-fsm (make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep)))
(define log-string (open-output-string))
(with-logging-to-port
    log-string
  (lambda ()
    (let* ([exec (make-machine-execution simple-fsm)]
           [started (execution-start exec)]
           [next1 (handle-event started 'sleep)]
           [next2 (handle-event next1 'wake)])
      (void)))
  'info)
(for-each displayln
          (map (curryr list-tail 3)
               (map (λ (line) (string-split line " " #:repeat? #t))
                    (string-split (get-output-string log-string) "\n" #:repeat? #t))))
]

State machine execution details:

@itemlist[
  @item{call @racket[execution-start] (created) to start @racket['start] state}
  @item{state machine advances on @racket[handle-event] or @racket[complete-event]}
  @item{completion = @racket['final] or terminal state}
  @item{event consumption/errors}
  @item{transition selection}
  @item{Limitations (compared to UML for example):
        @itemlist[
          @item{The procedure @racket[hand-event] is synchronous, the  event queues}
        ]}
]

@;{============================================================================}
@subsection[#:tag "fsm:types"]{Types and Predicates}

@defstruct*[state-machine
  ([name symbol?]
   [states (hash/c 'symbol state?)]
   [events (hash/c 'symbol (listof transition?))]
   [transitions (hash/c 'symbol (listof transition?))])]{
The state machine definition, returned by @racket[make-state-machine], and which
defines the states and transitions and has been validated to be correct. The
three @racket[hash] values are constructed as optimization for the execution and
comprise @racket[name] to @racket[state], @racket[event] to @racket[transition],
and source @racket[state] to @racket[transition] respectively.
}

@defstruct*[state
  ([name symbol?]
   [kind (or/c 'start 'normal 'final)]
   [entry (-> machine-execution? void?)]
   [execution (-> machine-execution? void?)]
   [exit (-> machine-execution? void?)])]{
}

@defstruct*[transition
  ([source-name symbol?]
   [target-name symbol?]
   [internal boolean?]
   [trigger-event symbol?]
   [guard (-> machine-execution? symbol? boolean?)]
   [effect (-> machine-execution? void?)])]{
}

@defstruct*[machine-execution
  ([model state-machine?]
   [condition (or/c 'created 'active 'in-error 'complete)]
   [current-state state?]
   [reporter (-> history-event? void?)])]{
}

@defstruct*[history-event
  ([current-execution machine-execution?]
   [time real?]
   [kind symbol?]
   [source (or/c #f state?)]
   [transition (or/c #f transition?)]
   [evaluations (listof string?)])
           #:transparent]{
}

@;{============================================================================}
@subsection[#:tag "fsm:construction"]{Construction}

@defproc[(make-state-machine
          [name symbol?]
          [states (non-empty-listof state?)]
          [transitions (non-empty-listof transition?)]
          [additional-events (non-empty-listof symbol?) '()])
          state-machine?]{
}

@defproc[(make-state
          [name symbol?]
          [kind (or/c 'start 'normal 'final)]
          [#:on-entry entry (-> machine-execution? void?) no-behavior]
          [#:execute execution (-> machine-execution? void?) no-behavior]
          [#:on-exit exit (-> machine-execution? void?) no-behavior])
         state?]{
}

@defproc[(make-transition
          [source-name symbol?]
          [target-name symbol?]
          [internal boolean? #f]
          [#:on-event trigger-event (or/c #f symbol?) #f]
          [#:guard guard (-> machine-execution? symbol? boolean?) no-guard]
          [#:execute effect (-> machine-execution? void?) no-behavior])
         transition?]{
}

@defthing[no-behavior (-> machine-execution? void?)]{
}

@defthing[no-guard (-> machine-execution? transition? boolean?)]{
}

@;{============================================================================}
@subsection[#:tag "fsm:execution"]{Execution}

@defproc[(make-machine-execution
          [from-machine state-machine?]
          [reporter (or/c #f(-> history-event? void?)) #f])
         machine-execution?]{
}

@defproc[(execution-start
          [exec machine-execution?])
         machine-execution?]{
}

@defproc[(handle-event
          [exec machine-execution?]
          [event symbol?])
         machine-execution?]{
}

@defproc[(complete-current-state
          [exec machine-execution?])
          machine-execution?]{
}
 
@defproc[(make-logging-reporter
          [a-logger logger? (current-logger)])
         (-> history-event? void?)]{
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module behavior/markov-chain}
@defmodule[behavior/markov-chain]

This module provides an implementation for (discrete-time, classic) Markov chains,
these can be used to create the underlying event stream, with specific distributions,
for higher-level behaviors, or for direct simulation. It also separates the
definition of a @racket[mkchain] from an execution instance so that the definition
may be reused across multiple executions. Individual states are represented as
symbols, the chain is represented as a matrix, with a row representing transition
sources and columns representing the transition target. Probabilities are represented
as real numbers from 0.0 to 1.0 and all probabilities in a row must add up to
either 0.0 or 1.0.

As defined by @hyperlink["https://en.wikipedia.org/wiki/Markov_chain" "Wikipedia"];
A Markov chain is "@italic{a stochastic model describing a sequence of possible
events in which the probability of each event depends only on the state attained
in the previous event}".

@examples[ #:eval example-eval
(define a-chain (make-chain 
                 (==> 'a (--> 'a .5) (--> 'b .25) (--> 'c .25))
                 (==> 'b (--> 'a .5) (--> 'c .5))
                 (==> 'c (--> 'a .25) (--> 'b .25) (--> 'c .5))))
(define an-exec (make-chain-execution a-chain 'b))
(execute an-exec 10)
(displayln (execution-trace an-exec))
(displayln (mkchain->graph-string a-chain))
]

The @racket[execution-trace] function returns the history of the execution as a
list of states the execution has been in. The list is ordered from most recent to
last; this trace is not a memory, the implementation is still a @italic{classic}
Markov chain.

@;{============================================================================}
@subsection[#:tag "chain:types"]{Types and Predicates}

@defthing[#:kind "contract" mkchain? contract?]{
Determines whether a contract parameter is a @italic{Markov chain} structure.
}

@defthing[#:kind "contract" mkchain-row? contract?]{
Determines whether a contract parameter is a @italic{Markov chain} row.
}

@defthing[#:kind "contract" mkchain-reporter? contract?]{
Determines whether a contract parameter is a @italic{markov chain} reporter
 function. Such functions can be used as callbacks when a state transition
 occurs during execution of a chain.
}

@defproc[(-->?
          [chain mkchain?]
          [from-state symbol?]
          [to-state symbol?])
         boolean?]{
Does a transition exist between @racket[from-state] and @racket[to-state]?.
}
  
@defproc[(<-->?
          [chain mkchain?]
          [state1 symbol?]
          [state2 symbol?])
         boolean?]{
Do the two states @racket[state1] and @racket[state2] @italic{communicate}?                   
Wikipedia: "@italic{A state i is said to @bold{communicate} with state j
  (written i ↔ j) if both i → j and j → i}". 
}
  
@defproc[(>--<?
          [chain mkchain?]
          [state symbol?])
         boolean]{
Is the state @racket[state] an @italic{absorbing} state?
Wikipedia: "@italic{A state i is called @bold{absorbing} if it is impossible to 
  leave this state}". This implies either no transitions exit this state, or
  the only transition from this state is to itself.
}

@;{============================================================================}
@subsection[#:tag "chain:construction"]{Construction}

@defproc[(==>
          [from-state symbol?]
          [pair pair?] ...+)
         pair?]{
Construct a pair that represents a row in the transition matrix. The pair c
consists of a symbol naming a state and one or more transition pairs (see
@racket[-->] below).
}

@defproc[(==>!
          [from-state symbol?])
         pair?]{
Construct a pair that represents an @italic{absorbing} state in the transition
 matrix.
}

@defproc[(-->
          [to-state symbol]
          [probability real?])
         pair?]{
Construct a transition pair that represents the target of a transition and it's
 associated probability.
}
  
@defproc[(make-chain
          [pair (pairof symbol? mkchain-row?)] ...+)
         (or/c #f mkchain?)]{
Make a new @racket[mkchain] structure from the list of symbol and row pairs.
 Each pair represents a source state and it's set of target state probabilities.
 These pairs can be constructed using the @racket[==>] and @racket[==>] functions.

@examples[ #:eval example-eval
(make-chain (==> 'a (--> 'a .3) (--> 'b .7))
            (==> 'b (--> 'b .7) (--> 'c .3))
            (==>! 'c))
]

This function will return @racket[#f] if any of the provided pairs are invalid.
}

@defproc[(make-diagonal-chain
          [state symbol?] ...+)
         mkchain?]{
Make a new @racket[mkchain] where the only transitions are the self-transitions
 for each state. The name of this function comes from visualizing the transition
 matrix as having only cells for the diagonal relationships where the from state
 and to state are the same.
}

@defproc[(row-ref
          [chain mkchain?]
          [from-state symbol?])
         mkchain-row?]{
Return a row from the transition matrix corresponding to the state @racket[from-state].
}

@defproc[(row-set
          [chain mkchain?]
          [state symbol?]
          [row mkchain-row?])
         (or/c #f mkchain?)]{
Set a row in the transition matrix corresponding to the state @racket[from-state],
 returning the new chain. If the row itself is invalid in any way the response
 is @racket[#f] and the chain is unchanged.
}

@defproc[(chain-states
          [chain mkchain?])
         (listof string?)]{
Return the list of symbols representing the states of this chain.
}

@;{============================================================================}
@subsection[#:tag "chain:execution"]{Execution}

An execution takes place according to discrete steps. At each step the set of
transitions @italic{from} the current state (see @racket[execution-state]) and chooses
a single transition based on the probabilities for each. Once chosen the @italic{to}
state is made current and the step is complete.

If the current state is an @italic{absorbing} state (see @racket[>--<?]) then the
execution is said to be complete (see @racket[execution-complete?]) and any further
calls to either @racket[execute] or @racket[execute-next] will have no effect.

@defproc[(make-chain-execution
          [from-chain mkchain?]
          [start-state symbol?]
          [reporter mkchain-reporter? #f])
         (or/c #f execution?)]{
Create an @racket[execution] structure from the given chain, and given starting
 state. The behavior of the execution depends on whether the execution itself
 keeps track of the execution history or whether the caller does by providing
 a @racket[reporter] function.

@itemlist[
 @item{When @racket[reporter] is specified the provided function is called with
  the new state each time one is selected. As the execution is not responsible
  for tracking the history of the chosen states, the value of the function
  @racket[execution-trace] is simply the current state.}
 @item{Otherwise, all states are recorded and can be retrieved by the
  @racket[execution-trace] function.}
]

This function will return @racket[#f] if @racket[start-state] is not a state within
the chain @racket[from-chain].
}

@defproc[(make-chain-execution-generator
          [from-chain mkchain?]
          [start-state symbol?])
         (or/c #f generator?)]{
Create an @racket[execution] generator from the given chain, and given starting
 state. The most common usage is in conjunction with @racket[in-producer] and use
 a stop-value of @racket[#f], as shown below.

@examples[ #:eval example-eval
(define d-chain (make-chain
                 (==> 'a (--> 'b 1.0))
                 (==> 'b (--> 'c 1.0))
                 (==>! 'c)))
(define next (make-chain-execution-generator d-chain 'a))
(for ([state (in-producer next #f)])
  (displayln state))
]

This function will return @racket[#f] if @racket[start-state] is not a state
 within the chain @racket[from-chain].
}

@defproc[(execute
          [exec execution?]
          [steps exact-positive-integer?])
         execution?]{
This function will perform a number of @racket[steps], effectively calling the
 @racket[execute-next] for each step. The response is the new state of the
 execution.
}

@defproc[(execute-next
          [exec execution?])
         execution?]{
Calculate the next state, store it in the execution trace, and return an updated
copy of the execution.
}

@defproc[(execution-trace
          [exec execution?])
         (listof symbol?)]{
Return the execution trace as a list of symbols representing the history of all
states the chain has been in.
}

@defproc[(execution-state
          [exec execution?])
         symbol?]{
Return the current state the chain is in.
}

@defproc[(execution-complete?
          [exec execution?])
         boolean?]{
Return @racket[#t] if the execution has reached an @italic{absorbing}
 state (see @racket[>--<?]).
}
@;{============================================================================}
@subsection[#:tag "chain:graphviz"]{GraphViz}

It can be very useful to view a Markov chain as a state transition diagram and
the following functions provide this capability by writing the DOT file format
used by @hyperlink["https://graphviz.org/" "Graphviz"] for visualization.

Given the example in this module's introduction, running the resulting DOT text
through @tt{dot -T png a-chain.dot > a-chain-graph.png} results in the following
image.

@centered{@image{scribblings/a-chain-graph.png}}

@defproc[(mkchain->graph
          [chain mkchain?]
          [out port?])
         void?]{
Write a Graphviz representation of the provided chain to the provided output port.
}

@defproc[(mkchain->graph-string
          [chain mkchain?])
         string?]{
Return a Graphviz representation of the provided chain as a string.
}

