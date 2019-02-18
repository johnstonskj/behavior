# Behavioral Models for Racket

[![GitHub release](https://img.shields.io/github/release/johnstonskj/behavior.svg?style=flat-square)](https://github.com/johnstonskj/behavior/releases)
[![Travis Status](https://travis-ci.org/johnstonskj/behavior.svg)](https://www.travis-ci.org/johnstonskj/behavior)
[![Coverage Status](https://coveralls.io/repos/github/johnstonskj/behavior/badge.svg?branch=master)](https://coveralls.io/github/johnstonskj/behavior?branch=master)
[![raco pkg install behavior](https://img.shields.io/badge/raco%20pkg%20install-behavior-blue.svg)](http://pkgs.racket-lang.org/package/behavior)
[![Documentation](https://img.shields.io/badge/raco%20docs-behavior-blue.svg)](http://docs.racket-lang.org/behavior/index.html)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/behavior.svg)](https://github.com/johnstonskj/behavior/stargazers)
![MIT License](https://img.shields.io/badge/license-MIT-118811.svg)

This package introduces a set of modeling techniques for exploring behavior in software systems.
Models such as state machines and Petri nets can be used to model the behavior of a system, and
Markov chains be used to generate events to help in simulation of systems.

# Modules

* `fsm` -- UML-style State Machines including entry/exit/do behaviors and transition effects and guards.
* `markov-chain` -- Define the state/transition matrix for a Markov chain and execute the chain
  resulting in a stream of state events.
* `petri-net` -- Simple Petri net model and execution.
* `reporters` -- common mechanisms for tracking events in model execution.  

# Examples

Define a state machine, a simple one, a start and an end with a transition triggered by a single event.

```racket
(make-state-machine
 'first-fsm
 (list (make-state 'hello 'start)
       (make-state 'goodbye 'final))
 (list (make-transition 'hello 'goodbye #:on-event 'wake)))
```

Execute the state machine.

```racket
(let* ([exec (make-machine-execution simple-fsm (make-logging-reporter))]
       [started (machine-execution-start exec)]
       [next1 (handle-event started 'sleep)]
       [next2 (handle-event next1 'wake)])
      (state-machine-complete? next2))
```

Define a Markov chain with 3 states.

```racket
(define a-chain (make-chain 
                 (==> 'a (--> 'a .5) (--> 'b .25) (--> 'c .25))
                 (==> 'b (--> 'a .5) (--> 'c .5))
                 (==> 'c (--> 'a .25) (--> 'b .25) (--> 'c .5))))
```

Execute the chain with 10 steps and display the history of events.

```racket
(define an-exec (make-execution a-chain 'b))
(execute an-exec 10)
(displayln (execution-trace an-exec))
(displayln (mkchain->graph-string a-chain))
```

Define a simple Petri net, two places, one transition.

```racket
(define net (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'a 't 1)
                                  (make-arc 't 'b 1))))
```

Execute the Petri net with an initial configuration that has a single token at place "a".

```racket
(define exec (make-net-execution net (hash 'a 1)))
(execute-net exec)
 ```

[![Racket Language](https://racket-lang.org/logo-and-text-1-2.png)](https://racket-lang.org/)
