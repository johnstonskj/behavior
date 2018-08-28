#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          behavior/fsm
          behavior/markov-chain
          behavior/petri-net
          behavior/reporter
          (for-label racket/contract
                     racket/logging
                     behavior/fsm
                     behavior/markov-chain
                     behavior/petri-net
                     behavior/reporter))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/function
                                racket/logging
                                racket/set
                                racket/string
                                behavior/fsm
                                behavior/markov-chain
                                behavior/petri-net
                                behavior/reporter)))

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
  @item{Model validation rules (resulting in @racket[exn:fail:contract] errors
        from @racket[make-state-machine]):
        @itemlist[
          @item{The model must have one, and only one, start state.}
          @item{The model must have one, and only one, final state.}
          @item{Transitions marked as internal must have the same source and
                target state.}
          @item{A state may only have one outgoing, @italic{unguarded}, transition.}
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
    (let* ([exec (make-machine-execution simple-fsm (make-logging-reporter))]
           [started (machine-execution-start exec)]
           [next1 (handle-event started 'sleep)]
           [next2 (handle-event next1 'wake)])
      (void)))
  'info)
(for-each displayln
          (map (curryr list-tail 3)
               (map (λ (line) (string-split line " " #:repeat? #t))
                    (string-split (get-output-string log-string) "\n" #:repeat? #t))))
]

A state machine is executed by constructing a @racket[machine-execution] and then
calling @racket[execution-start]. This will transition to the start state and
perform any behaviors associated with it. The state machine only transitiona from
one state to another in response to an event (via @racket[handle-event]) or by
state completion (via @racket[complete-current-state]). The state machine is completed
when it transitions into either a @racket['final] state, or a state with no
outgoing transitions.

Additional execution details:

@itemlist[
  @item{A new execution has a condition @racket['created], and will not accept
        @racket[handle-event] or @racket[complete-current-state] calls until in
        condition @racket['active].}
  @item{The call to @racket[execution-start] will alter the condition to
        @racket['active] and set the current state to the start state.}
  @item{Once the execution completes the condition is set to @racket['complete]
        and the state machine will again reject calls to  @racket[handle-event]
        or @racket[complete-current-state].}
  @item{When handling an event (if the event is valid for this machine) if no
        transition leaving the current state is triggered by the event it is
        logged and ignored. If more than one transition is triggered by the
        event @italic{and} they are all enabled an error occurs and the
        execution condition is set to @racket['in-error].}
  @item{Similarly, when calling @racket[complete-current-state] if no transitions are
        enabled at this time, or if multiple transitions are enabled, an error
        occurs.}
  @item{The last two situations are considered temporary, while the machine
        indicates an error the actions can be re-tried if a different event
        is used, or of guard conditions evaluate to different results. In the
        case where a single transition is enabled on retry the execution
        condition is reset to @racket['active].}
  @item{Reporters are supported that receive events from the running execution
        for logging or other behavior.}
  @item{Limitations (compared to UML for example):
        @itemlist[
          @item{The procedure @racket[handle-event] is synchronous, no queue is
                provided for incoming events.}
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
Represents a state within the @racket[state-machine], also included in certain
@racket[machine-history-event] instances.
}

@defstruct*[transition
  ([source-name symbol?]
   [target-name symbol?]
   [internal boolean?]
   [trigger-event symbol?]
   [guard (-> machine-execution? symbol? boolean?)]
   [effect (-> machine-execution? void?)])]{
Represents a transition within the @racket[state-machine], also included in certain
@racket[machine-history-event] instances.
}

@defstruct*[machine-execution
  ([model state-machine?]
   [condition (or/c 'created 'active 'in-error 'complete)]
   [current-state state?])]{
This represents the current state of a state machine execution, note that calls
to @racket[handle-event] and @racket[complete-current-state] return new copies
of this structure. 
}

@defstruct*[(machine-history-event history-event)
  ([current-execution machine-execution?]
   [kind symbol?]
   [source (or/c #f state?)]
   [transition (or/c #f transition?)]
   [evaluations (listof string?)])
           #:transparent]{
These events are sent to the reporter associated with discrete actions taken as
part of the state machine execution. For example, @racket[kind] may be one of
@racket['starting], @racket['enter-state], @racket['execute-state],
@racket['exit-state], @racket['handle-event], @racket['transition], 
@racket['transition-effect], or @racket['completed].
}

@;{============================================================================}
@subsection[#:tag "fsm:construction"]{Construction}

@defproc[(make-state-machine
          [name symbol?]
          [states (non-empty-listof state?)]
          [transitions (non-empty-listof transition?)]
          [additional-events (listof symbol?) '()])
          state-machine?]{
Construct a new @racket[state-machine], all validation of the model is performed
during construction and reported as @racket[exn:fail:contract] exceptions.
}

@defproc[(make-state
          [name symbol?]
          [kind (or/c 'start 'normal 'final)]
          [#:on-entry entry (-> machine-execution? void?) no-behavior]
          [#:execute execution (-> machine-execution? void?) no-behavior]
          [#:on-exit exit (-> machine-execution? void?) no-behavior])
         state?]{
Construct a new @racket[state].
}

@defproc[(make-transition
          [source-name symbol?]
          [target-name symbol?]
          [internal boolean? #f]
          [#:on-event trigger-event (or/c #f symbol?) #f]
          [#:guard guard (-> machine-execution? symbol? boolean?) no-guard]
          [#:execute effect (-> machine-execution? void?) no-behavior])
         transition?]{
Construct a new @racket[transition], note that by using symbols for the
@racket[sourvce-name] and @racket[target-name] we do not need to reference
@racket[state] instances directly and the construction is considerably
easier to read/understand.

@examples[ #:eval example-eval
(make-state-machine
 'first-fsm
 (list (make-state 'hello 'start)
       (make-state 'goodbye 'final))
 (list (make-transition 'hello 'goodbye #:on-event 'wake)))
]
}

@defthing[no-behavior (-> machine-execution? void?)]{
A default behavior implementation that does nothing.
}

@defthing[no-guard (-> machine-execution? transition? boolean?)]{
A default guard implementation that simply returns @racket[#t].
}

@;{============================================================================}
@subsection[#:tag "fsm:execution"]{Execution}

@defproc[(make-machine-execution
          [from-machine state-machine?]
          [reporter (or/c #f (-> machine-history-event? void?)) #f])
         machine-execution?]{
Construct a new @racket[machine-execution] using @racket[from-machine] as the
definition. The returned execution will be in the @racket['created] condition.
}

@defproc[(machine-execution-start
          [exec machine-execution?])
         machine-execution?]{
Transition to the start state of the machine. The returned execution will be in
the @racket['active] condition.
}

@defproc[(machine-execution-complete?
          [exec machine-execution?])
         boolean?]{
Returns @racket[#t] if the execution is in the @racket['completed] condition.
}

@defproc[(handle-event
          [exec machine-execution?]
          [event symbol?])
         machine-execution?]{
Consume the @racket[event] and determine the next action. The returned execution
will be in either the @racket['active], @racket['in-error], or @racket['completed]
condition.
}

@defproc[(complete-current-state
          [exec machine-execution?])
          machine-execution?]{
Complete the current state and determine the next action. The returned execution
will be in either the @racket['active], @racket['in-error], or @racket['completed]
condition.
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

@defstruct*[mkchain
  ([name symbol?])]{
}

@defstruct*[chain-execution
  ([model symbol?]
   [state symbol?]
   [complete? boolean?])]{
}

@defstruct*[(chain-history-event history-event)
  ([current-execution chain-execution?]
   [state symbol?])]{
}

@defthing[#:kind "contract" mkchain-row? contract?]{
Determines whether a contract parameter is a @italic{Markov chain} row.
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
          [name symbol?]
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
          [name symbol?]
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

@defproc[(mkchain-states
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

@defproc[(execute-chain
          [exec execution?]
          [steps exact-positive-integer?])
         execution?]{
This function will perform a number of @racket[steps], effectively calling the
 @racket[execute-next] for each step. The response is the new state of the
 execution.
}

@defproc[(execute-chain-step
          [exec execution?])
         execution?]{
Calculate the next state, store it in the execution trace, and return an updated
copy of the execution.
}

@defproc[(execution-chain-state
          [exec execution?])
         symbol?]{
Return the current state the chain is in.
}

@defproc[(execution-chain-complete?
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

@;{============================================================================}
@;{============================================================================}
@section[]{Module behavior/petri-net}
@defmodule[behavior/petri-net]

TBD

@examples[ #:eval example-eval
(define net (make-petri-net 'first-net
                            (set 'a 'b)
                            (set 't)
                            (set (make-arc 'a 't 1)
                                 (make-arc 't 'b 1))))
(define reporter (λ (ev)
                   (displayln
                    (format "~a ~a ~a"
                            (net-history-event-place-or-transition ev)
                            (net-history-event-kind ev)
                            (net-history-event-tokens ev)))))
(define exec (make-net-execution net (hash 'a 1) reporter))
(execute-net exec)
]

@;{============================================================================}
@subsection[#:tag "petri:types"]{Types and Predicates}

@defstruct*[petri-net
  ([name symbol?]
   [colored? boolean?])]{
}

@defstruct*[arc
  ([from symbol?]
   [to symbol?]
   [threshold exact-nonnegative-integer?])]{
}

@defstruct*[net-execution
  ([model petri-net?]
   [place-tokens (hash/c symbol (listof symbol?))])]{
}

@defstruct*[(net-history-event history-event)
  ([current-execution net-execution?]
   [kind symbol?]
   [place-or-transition symbol?]
   [tokens list?])]{
}


@;{============================================================================}
@subsection[#:tag "petri:construction"]{Construction}


@defproc[(make-petri-net
          [name symbol?]
          [places (set/c symbol?)]
          [transitions (set/c symbol?)]
          [arcs (set/c arc?)]
          [#:inhibitors? inhibitors? boolean? #f])
        petri-net?]{
}
  
@defproc[(make-arc
          [from symbol?]
          [to symbol?]
          [threshold exact-nonnegative-integer?])
         arc?]{
}

@;{============================================================================}
@subsection[#:tag "petri:execution"]{Execution}


@defproc[(make-net-execution
          [model petri-net?]
          [configuration (hash/c symbol? (or/c exact-nonnegative-integer? (listof symbol?)))]
          [reporter (-> net-history-event? void?) #f])
         net-execution?]{
}

@defproc[(execute-net
         [exec net-execution?])
         net-execution?]{
}

@defproc[(execute-net-step
          [exec net-execution?])
         net-execution?]{
}

@defproc[(net-execution-complete?
          [exec net-execution?])
         boolean?]{
}

@;{============================================================================}
@;{============================================================================}
@section[]{Module behavior/reporter}
@defmodule[behavior/reporter]

This module provides a set of functions that construct @racket[reporter] functions
used be the models above. Effectively, each of the behavior models will emit
@racket[history-event]s that correspond to changes in the execution state.

To this end the @italic{make-?-execution} functions will take a @italic{reporter}
optional parameter. In general, if no reporter is specified, the null
(@racket[make-null-reporter]) reporter is used.

@defstruct*[history-event
  ([time real?])
            #:transparent]{
These events are sent to a reporter function to denote an action taken place
within a behavior execution. These structures cannot be created directly,
it is intended as the parent of behavior-specific history events.}

@defproc[(make-null-reporter)
         (-> history-event? void?)]{
Creates a reporter that consumes all events and does nothing.
}

@defproc[(make-buffering-reporter
          [selector (-> history-event? any/c)])
         (values (-> list?)
                 (-> history-event? void?))]{
This function creates a reporter that buffers all the events sent to it. It
returns two values; the first if a function that returns the current buffer
as a list, the second is the @racket[reporter] function itself. The @racket[selector]
function is called to buffer a subset of the passed event, else the entire event is
buffered.
}

@defproc[(make-channel-reporter)
         (values channel? (-> history-event? void?))]{
Returns two values; the first is a newly created @racket[channel] and the
second is a @racket[reporter] function that will accept events and write them to
this channel. 
}

@defproc[(make-logging-reporter
          [a-logger logger? (current-logger)])
         (-> history-event? void?)]{
Returns a @racket[reporter] function that will accept events and write them to a
standard racket @racket[logger].
}

@defproc[(make-port-reporter
          [port output-port?])
         (-> history-event? void?)]{
Returns a @racket[reporter] function that will accept events and write them to a
standard racket @racket[output-port?].
}

