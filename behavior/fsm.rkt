#lang racket
;;
;; Behavioral Models -- Finite State Machine.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [make-state-machine
   (->* (symbol?
         (non-empty-listof state?)
         (non-empty-listof transition?))
        ((non-empty-listof symbol?))
       state-machine?)]

  [make-state
   (->* (symbol? (or/c 'start 'normal 'final))
        (#:on-entry (-> machine-execution? void?)
         #:execute (-> machine-execution? void?)
         #:on-exit (-> machine-execution? void?))
        state?)]

  [make-transition
   (->* (symbol? symbol?)
        (boolean?
         #:on-event symbol?
         #:guard (-> machine-execution? symbol? boolean?)
         #:execute (-> machine-execution? void?))
        transition?)]

  [make-machine-execution
   (->* (state-machine?) ((-> machine-history-event? void?)) machine-execution?)]

  [execution-start
   (-> machine-execution? machine-execution?)]

  [handle-event
   (-> machine-execution? symbol? machine-execution?)]

  [complete-current-state
   (-> machine-execution? machine-execution?)])

 (except-out
  (struct-out state-machine)
  private-state-machine)

 (except-out
  (struct-out state)
  private-state)

 (except-out
  (struct-out transition)
  private-transition)

 (except-out
  (struct-out machine-execution)
  private-machine-execution
  machine-execution-reporter)
 
 (except-out
  (struct-out machine-history-event)
  private-history-event))

;; ---------- Requirements

(require
  behavior/reporter)

;; ---------- Internal types

(struct state-machine
  (name
   states ; name -> state
   events ; name -> transition(s)
   transitions) ; state-name -> transition(s)
  #:constructor-name private-state-machine)

(struct state
  (name
   kind
   entry
   execution
   exit)
  #:constructor-name private-state)

(struct transition
  (source-name
   target-name
   internal
   trigger-event
   guard
   effect)
  #:constructor-name private-transition)

(struct machine-execution
  (model
   condition ; created active in-error complete
   current-state
   reporter)
  #:constructor-name private-machine-execution)

(struct machine-history-event history-event
  (current-execution
   kind
   source
   transition
   evaluations)
  #:constructor-name private-history-event
  #:transparent)

;; ---------- Implementation - Definition

(define (no-behavior exec) (void))

(define (no-guard exec event) #t)

(define internal-event (gensym))

(define (make-state-machine name states transitions [additional-events '()])
  (unless (= (length (filter (λ (s) (equal? (state-kind s) 'start)) states)) 1)
    (raise-argument-error 'make-state-machine "Requires one, and only one, start state" states))
  (unless (= (length (filter (λ (s) (equal? (state-kind s) 'final)) states)) 1)
    (raise-argument-error 'make-state-machine "Requires one, and only one, final state" states))
  (define state-hash (for/hash ([state states]) (values (state-name state) state)))
  (for ([t transitions])
    (unless (hash-has-key? state-hash (transition-source-name t))
      (raise-argument-error 'make-state-machine
                            "Transition's source state must be in state machine" t))
    (unless (hash-has-key? state-hash (transition-target-name t))
      (raise-argument-error 'make-state-machine
                            "Transition's target state must be in state machine" t)))

  (define transition-hash
    (for/hash ([state states])
      (values (state-name state)
              (filter (λ (t) (equal? (transition-source-name t) (state-name state))) transitions))))
  (for ([(state transitions) transition-hash])
    (define unguarded (filter (λ (t) (equal? (transition-guard t) no-guard)) transitions))
    (when (> (length unguarded) 1)
      (raise-argument-error 'make-state-machine
                            "State has more than one outgoing, unguarded, transitions" state)))
  
  (define event-set (set-union (for/set ([transition transitions])
                                 (transition-trigger-event transition))
                               (list->set additional-events)))
  (define event-hash
    (for/hash ([ev event-set])
                  (values ev (filter (λ (t) (equal? (transition-trigger-event t) ev)) transitions))))

  (private-state-machine
   name
   state-hash
   event-hash
   transition-hash))

(define (make-state name kind
                    #:on-entry [entry no-behavior]
                    #:execute [execution no-behavior]
                    #:on-exit [exit no-behavior])
  (private-state name kind entry execution exit))

(define (make-transition source-name
                         target-name
                         [internal #f]
                         #:on-event [trigger-event #f]
                         #:guard [guard no-guard]
                         #:execute [effect no-behavior])
  (when (and internal (not (equal? source-name target-name)))
      (raise-argument-error 'make-transition
                            "Internal transitions must be self-transitions"
                            (format "~a --> ~a" source-name target-name)))
  (private-transition source-name target-name internal trigger-event guard effect))

;; ---------- Implementation - Execution

(define (make-machine-execution state-machine [reporter #f])
  (private-machine-execution state-machine
                     'created
                     #f
                     (if reporter
                         reporter
                         (make-logging-reporter))))

(define (execution-start exec)
  (unless (equal? (machine-execution-condition exec) 'created)
    (raise-argument-error 'execution-start "Cannot call execution-start more than once" 'start))
  (define start-state
    (first (filter (λ (s) (equal? (state-kind s) 'start))
                   (hash-values (state-machine-states (machine-execution-model exec))))))
  (report-starting exec start-state)
  (define start (update-execution-active exec))
  (define started (enter-state start start-state))
  started)

(define (handle-event exec event)
  (check-current-condition 'handle-event exec)
  (unless (hash-has-key? (state-machine-events (machine-execution-model exec)) event)
    (raise-argument-error 'handle-event "Event not part of state machine model" event))
  
  (define transitions
    (filter
     (λ (t) (or (false? (transition-guard t)) ((transition-guard t) exec event)))
     (filter (λ (t) (equal? (transition-source-name t)
                            (state-name (machine-execution-current-state exec))))
             (hash-ref (state-machine-events (machine-execution-model exec)) event))))
  (cond
    [(equal? transitions '())
     (begin
       (report-event exec event #f)
       (update-execution-error exec))]
    [(= (length transitions) 1)
     (let ([actual (first transitions)])
       (report-event exec event actual)
       (exit-state (if (equal? (machine-execution-condition exec) 'in-error)
                       (update-execution-active exec)
                       exec)
                   actual
                   event))]
    [(> (length transitions) 1)
     (begin
       (report-event-error exec event transitions)
       (update-execution-error exec))]))

(define (complete-current-state exec)
  (check-current-condition 'complete-current-state exec)
  
  (define transitions
    (filter
     (λ (t) (or (false? (transition-guard t)) ((transition-guard t) exec internal-event)))
     (hash-ref
      (state-machine-transitions (machine-execution-model exec))
      (state-name (machine-execution-current-state exec)))))

  (cond
    [(equal? transitions '())
     (begin
       (report-complete-error exec transitions)
       (update-execution-error exec))]
    [(= (length transitions) 1)
     (exit-state (if (equal? (machine-execution-condition exec) 'in-error)
                     (update-execution-active exec)
                     exec)
                 (first transitions))]
    [(> (length transitions) 1)
     (begin
       (report-complete-error exec transitions)
       (update-execution-error exec))]))

;; ---------- Internal procedures

(define (check-current-condition who exec)
  (when (member (machine-execution-condition exec) '(created completed))
    (raise-argument-error who "Cannot execute in condition" (machine-execution-condition exec))))

(define (enter-state exec state)
  (define exec2 (update-execution-state exec state))
  (define entry-behavior (state-entry (machine-execution-current-state exec2)))
  (entry-behavior exec2)
  (report-entered-state exec entry-behavior)
  (define execution-behavior (state-execution (machine-execution-current-state exec2)))
  (execution-behavior exec2)
  (report-executed-state exec execution-behavior)
  (if (or (member (state-kind state) '(final))
          (= (length
              (hash-ref (state-machine-transitions
                         (machine-execution-model exec2)) (state-name state)))
             0))
      (let ([exec3 (update-execution-completed exec2)])
        (report-completed exec3)
        exec3)
      exec2))

(define (exit-state exec transition [event internal-event])
  (unless (transition-internal transition)
    (let ([exit-behavior (state-exit (machine-execution-current-state exec))])
      (report-exited-state exec exit-behavior)
      (exit-behavior exec)))
  (let* ([guard (transition-guard transition)]
         [result (guard exec event)])
    (report-transitioning exec transition guard result))
  ; do!
  (report-transitioned exec (transition-effect transition) transition)
  (define target (hash-ref (state-machine-states (machine-execution-model exec))
                           (transition-target-name transition)))
  (if (transition-internal transition)
      exec
      (enter-state exec target)))

(define (report-starting exec state)
  ((machine-execution-reporter exec)
   (private-history-event
    exec
    (current-inexact-milliseconds)
    'starting
    state
    #f
    '())))

(define (report-entered-state exec behavior)
  (report-executed-behavior exec 'enter-state behavior))

(define (report-executed-state exec behavior)
  (report-executed-behavior exec 'execute-state behavior))

(define (report-exited-state exec behavior)
  (report-executed-behavior exec 'exit-state behavior))

(define (report-transitioned exec behavior transition)
  (report-executed-behavior exec 'transition-effect behavior))

(define (report-executed-behavior exec kind behavior [transition #f])
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    kind
    (machine-execution-current-state exec)
    transition
    (if behavior (list (~a behavior)) '()))))

(define (report-event exec event transition)
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    'handle-event
    (machine-execution-current-state exec)
    transition
    (list (~a event)))))

(define (report-event-error exec event transitions)
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    'event-error
    (machine-execution-current-state exec)
    #f
    (cons (~a event) transitions))))

(define (report-transitioning exec transition guard guard-result)
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    'transition
    (machine-execution-current-state exec)
    transition
    (if guard (list (format "~a => ~a" guard guard-result)) '()))))

(define (report-complete-error exec transitions)
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    'complete-state-error
    (machine-execution-current-state exec)
    #f
    transitions)))

(define (report-completed exec)
  ((machine-execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    exec
    'completed
    (machine-execution-current-state exec)
    #f
    '())))

(define (update-execution-state exec new-state)
  (private-machine-execution
   (machine-execution-model exec)
   (machine-execution-condition exec)
   new-state
   (machine-execution-reporter exec)))

(define (update-execution-active exec)
  (private-machine-execution
   (machine-execution-model exec)
   'active
   (machine-execution-current-state exec)
   (machine-execution-reporter exec)))

(define (update-execution-error exec)
  (private-machine-execution
   (machine-execution-model exec)
   'in-error
   (machine-execution-current-state exec)
   (machine-execution-reporter exec)))

(define (update-execution-completed exec)
  (private-machine-execution
   (machine-execution-model exec)
   'completed
   (machine-execution-current-state exec)
   (machine-execution-reporter exec)))
