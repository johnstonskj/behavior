#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          behavior/markov-chain
          (for-label behavior/markov-chain
                     racket/contract))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require behavior/markov-chain)))

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
(define an-exec (make-execution a-chain 'b))
(execute an-exec 10)
(displayln (execution-trace an-exec))
(displayln (mkchain->graph-string a-chain))
]

The @racket[execution-trace] function returns the history of the execution as a
list of states the execution has been in. The list is ordered from most recent to
last; this trace is not a memory, the implementation is still a @italic{classic}
Markov chain.

@;{============================================================================}
@subsection[]{Types and Predicates}

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
@subsection[]{Construction}

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
@subsection[]{Execution}

An execution takes place according to discrete steps. At each step the set of
transitions @italic{from} the current state (see @racket[execution-state]) and chooses
a single transition based on the probabilities for each. Once chosen the @italic{to}
state is made current and the step is complete.

If the current state is an @italic{absorbing} state (see @racket[>--<?]) then the
execution is said to be complete (see @racket[execution-complete?]) and any further
calls to either @racket[execute] or @racket[execute-next] will have no effect.

@defproc[(make-execution
          [from-chain mkchain?]
          [start-state symbol?]
          [reporter mkchain-reporter? #f])
         execution?]{
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
}

@defproc[(make-execution-generator
          [from-chain mkchain?]
          [start-state symbol?])
         generator?]{
Create an @racket[execution] generator from the given chain, and given starting
 state.

@bold{Warning:} this function is untested at this time.
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
@subsection[]{GraphViz}

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

