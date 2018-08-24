#lang racket
;;
;; Behavioral Models -- Finite State Machine.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [make-state-machine
   (-> symbol?
       (non-empty-listof state?)
       (non-empty-listof symbol?)
       (non-empty-listof transition?)
       state-machine?)]

  [make-state
   (->* (symbol? (or/c 'start 'normal 'final))
        (#:on-entry (-> execution? void?)
         #:execute (-> execution? void?)
         #:on-exit (-> execution? void?))
        state?)]

  [make-transition
   (->* (symbol? symbol?)
        (boolean?
         #:on-event symbol?
         #:guard (-> execution? symbol? boolean?)
         #:execute (-> execution? void?))
        transition?)]

  [make-execution
   (->* (state-machine?) ((-> history-event? void?)) execution?)]

  [make-logging-reporter
   (-> logger? (-> history-event? void?))]

  [execution-start
   (-> execution? execution?)]

  [handle-event
   (-> execution? symbol? execution?)]

  [complete-current-state
   (-> execution? execution?)])
 
 (struct-out state-machine)
 (except-out private-state-machine)

 (struct-out state)
 (except-out private-state)

 (struct-out transition)
 (except-out private-transition)

 (struct-out execution)
 (except-out private-execution)

 (struct-out history-event)
 (except-out private-history-event))

;; ---------- Requirements

(require)

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

(struct execution
  (model
   condition ; created active in-error complete
   current-state
   reporter)
  #:constructor-name private-execution)

(struct history-event
  (time
   kind
   source
   transition
   evaluations)
  #:constructor-name private-history-event
  #:transparent)

;; ---------- Implementation - Definition

(define (no-behavior exec) (void))

(define (no-guard exec) #t)

(define (make-state-machine name states additional-events transitions)
  (unless (= (length (filter (λ (s) (equal? (state-kind s) 'start)) states)) 1)
    (raise-argument-error 'make-state-machine "Requires one, and only one, start state" states))
  (unless (= (length (filter (λ (s) (equal? (state-kind s) 'final)) states)) 1)
    (raise-argument-error 'make-state-machine "Requires one, and only one, final state" states))
  (define state-hash (for/hash ([state states]) (values (state-name state) state)))
  (for ([t transitions])
    (unless (hash-has-key? state-hash (transition-source-name t))
      (raise-argument-error 'make-state-machine "Source state must be in state machine" t))
    (unless (hash-has-key? state-hash (transition-target-name t))
      (raise-argument-error 'make-state-machine "Target state must be in state machine" t))
    (unless (hash-has-key? state-hash (transition-target-name t))
      (raise-argument-error 'make-state-machine "Trigger event must be in state machine" t)))

  (define transition-hash
    (for/hash ([state states])
      (values (state-name state)
              (filter (λ (t) (equal? (transition-source-name t) (state-name state))) transitions))))

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

(define (make-logging-reporter [a-logger (current-logger)])
  (λ (ev) (log-info (~s ev))))
  
(define (make-execution state-machine [reporter #f])
  (private-execution state-machine
                     'created
                     #f
                     (if reporter
                         reporter
                         (make-logging-reporter (current-output-port)))))

(define (execution-start exec)
  (unless (equal? (execution-condition exec) 'created)
    (raise-argument-error 'execution-start "Cannot call execution-start more than once" 'start))
  (define start-state
    (first (filter (λ (s) (equal? (state-kind s) 'start))
                   (hash-values (state-machine-states (execution-model exec))))))
  (report-starting exec start-state)
  (define start (update-execution-started exec))
  (define started (enter-state start start-state))
  started)

(define (handle-event exec event)
  (check-current-condition 'handle-event exec)
  (unless (hash-has-key? (state-machine-events (execution-model exec)) event)
    (raise-argument-error 'handle-event "Event not part of state machine model" event))
  (define transitions
    (filter (λ (t) (equal? (transition-source-name t) (state-name (execution-current-state exec))))
            (hash-ref (state-machine-events (execution-model exec)) event)))
  (cond
    [(equal? transitions '())
     (begin
       (report-event exec event #f)
       exec)]
    [(= (length transitions) 1)
     (let ([actual (first transitions)])
       (report-event exec event actual)
       (exit-state exec actual))]
    [(> (length transitions) 1)
     (begin
       (report-event exec event transitions)
       (error "indeterminate")
       exec)]))

(define (complete-current-state exec)
  (check-current-condition 'complete-current-state exec)
  (define transitions
    (filter
     (λ (t) (or (false? (transition-guard t)) ((transition-guard t) exec))
     (hash-ref
      (state-machine-transitions (execution-model exec))
      (state-name (execution-current-state exec))))))
  (cond
    [(equal? transitions '())
     (error "no valid transitions from state")]
    [(= (length transitions) 1)
     (exit-state exec (first transitions))]
    [(> (length transitions) 1)
     (error "indeterminate")]))

;; ---------- Internal procedures

(define (check-current-condition who exec)
  (when (member (execution-condition exec) '(created completed))
    (raise-argument-error who "Cannot execute in condition" (execution-condition exec))))

(define (enter-state exec state)
  (define exec2 (update-execution-state exec state))
  (define entry-behavior (state-entry (execution-current-state exec2)))
  (entry-behavior exec2)
  (report-entered-state exec entry-behavior)
  (define execution-behavior (state-execution (execution-current-state exec2)))
  (execution-behavior exec2)
  (report-executed-state exec execution-behavior)
  (when (or (member (state-kind state) '(final))
          (= (length
              (hash-ref (state-machine-transitions (execution-model exec2)) (state-name state)))
             0))
      (report-completed (update-execution-completed exec2)))
  exec2)

(define (exit-state exec transition)
  (unless (transition-internal transition)
    (let ([exit-behavior (state-exit (execution-current-state exec))])
      (exit-behavior exec)))
  (let* ([guard (transition-guard transition)]
         [result (guard exec)])
    (report-transitioning exec transition guard result))
  (if (transition-internal transition)
      exec
      (enter-state
       exec
       (hash-ref (state-machine-states (execution-model exec)) (transition-target-name transition)))))

(define (report-starting exec state)
  ((execution-reporter exec)
   (private-history-event
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

(define (report-executed-behavior exec kind behavior)
  ((execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    kind
    (execution-current-state exec)
    #f
    (if behavior (~a behavior) '()))))

(define (report-event exec event transition)
  ((execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    'handle-event
    (execution-current-state exec)
    transition
    (list (~a event)))))

(define (report-transitioning exec transition guard guard-result)
  ((execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    'transition
    (execution-current-state exec)
    transition
    (if guard (list (format "~a => ~a" guard guard-result)) '()))))

(define (report-completed exec)
  ((execution-reporter exec)
   (private-history-event
    (current-inexact-milliseconds)
    'completed
    (execution-current-state exec)
    #f
    '())))

(define (update-execution-state exec new-state)
  (private-execution
   (execution-model exec)
   (execution-condition exec)
   new-state
   (execution-reporter exec)))

(define (update-execution-started exec)
  (private-execution
   (execution-model exec)
   'started
   (execution-current-state exec)
   (execution-reporter exec)))

(define (update-execution-completed exec)
  (private-execution
   (execution-model exec)
   'completed
   (execution-current-state exec)
   (execution-reporter exec)))
