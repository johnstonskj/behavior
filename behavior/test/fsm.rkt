#lang racket
;;
;; Behavioral Models -- Finite State Machine.
;;
;;
;; ~ Simon Johnston 2018.

;; ---------- Requirements

(require rackunit
         racket/logging
         ; ---------
         behavior/fsm)

;; ---------- Test Fixtures

(define simple-fsm (make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep)))

(define blocked-fsm (make-state-machine
                     'first-fsm
                     (list (make-state 'hello 'start)
                           (make-state 'goodbye 'final))
                     (list (make-transition 'hello 'goodbye
                                            #:on-event 'wake
                                            #:guard (λ (ex ev) #f)))
                     '(sleep)))

(define indeterminate-fsm (make-state-machine
                           'first-fsm
                           (list (make-state 'hello 'start)
                                 (make-state 'goodbye 'final))
                           (list (make-transition 'hello 'goodbye
                                                  #:on-event 'wake)
                                 (make-transition 'hello 'goodbye
                                                  #:on-event 'wake
                                                  #:guard (λ (ex ev) #t)))
                           '(sleep)))

;; ---------- Internal procedures

(define (check-log-sequence log-string expected-states)
  (define log-list (string-split (get-output-string log-string) "\n" #:repeat? #t))
  (check-equal? (length log-list) (length expected-states))
  (for ([log-line log-list]
        [state expected-states])
    (check-equal? (fourth (string-split log-line " " #:repeat? #t))
                  (symbol->string state))))
  
;; ---------- Test Cases - Definition

(test-case
 "simple state machine: success"
 (define expected '(starting enter-state execute-state handle-event handle-event
                             exit-state transition transition-effect enter-state
                             execute-state completed))
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
 (check-log-sequence log-string expected))

(test-case
 "make-state-machine: fail on no start event"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'normal)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on two start events"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'start))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on no final event"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'normal))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on two final events"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'final)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on bad transition source"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hi 'goodbye #:on-event 'wake))
                    '(sleep))))))


(test-case
 "make-state-machine: fail on bad transition target"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'bye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on multiple unguarded"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'wait 'normal)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'wait)
                          (make-transition 'hello 'goodbye #:on-event 'wake))
                    '(sleep))))))

(test-case
 "make-state-machine: fail on bad internal transition"
 (check-exn exn:fail:contract?
            (λ ()
              ((make-state-machine
                    'first-fsm
                    (list (make-state 'hello 'start)
                          (make-state 'goodbye 'final))
                    (list (make-transition 'hello 'goodbye #t #:on-event 'wake))
                    '(sleep))))))

;; ---------- Test Cases - Execution

(test-case
 "make-machine-execution: success"
 (define exec (make-machine-execution simple-fsm))
 (check-equal? (machine-execution-model exec) simple-fsm)
 (check-equal? (machine-execution-condition exec) 'created)
 (check-false (machine-execution-current-state exec)))

(test-case
 "execution-start: success"
 (define exec (make-machine-execution simple-fsm))
 (define started (execution-start exec))
 (check-equal? (machine-execution-condition started) 'active)
 (check-equal? (state-name (machine-execution-current-state started)) 'hello))

(test-case
 "execution-start: fail on second call"
 (define exec (make-machine-execution simple-fsm))
 (define started (execution-start exec))
 (check-exn exn:fail:contract?
            (λ () (execution-start started))))

(test-case
 "handle-event: success"
 (define exec (make-machine-execution simple-fsm))
 (define started (execution-start exec))
 (define next (handle-event started 'wake))
 (check-equal? (machine-execution-condition next) 'completed)
 (check-equal? (state-name (machine-execution-current-state next)) 'goodbye))

(test-case
 "handle-event: fail on created"
 (define exec (make-machine-execution simple-fsm))
 (check-exn exn:fail:contract?
            (λ ()
              (handle-event exec 'wake))))

(test-case
 "handle-event: fail on unknown event"
 (define exec (make-machine-execution simple-fsm))
 (define started (execution-start exec))
 (check-exn exn:fail:contract?
            (λ ()
              (handle-event started 'snore))))

(test-case
 "handle-event: fail on no transition"
 (define exec (make-machine-execution blocked-fsm))
 (define in-error (handle-event (execution-start exec) 'wake))
 (check-equal? (machine-execution-condition in-error) 'in-error))


(test-case
 "handle-event: fail on multiple transitions"
 (define exec (make-machine-execution indeterminate-fsm))
 (define in-error (handle-event (execution-start exec) 'wake))
 (check-equal? (machine-execution-condition in-error) 'in-error))

(test-case
 "handle-event: success - recover from blocking error"
 (define guard (make-hash (list (cons 'say-yes #f))))
 (define recover-fsm (make-state-machine
                      'first-fsm
                      (list (make-state 'hello 'start)
                            (make-state 'goodbye 'final))
                      (list (make-transition 'hello 'goodbye
                                             #:on-event 'wake
                                             #:guard (λ (ex ev) (hash-ref guard 'say-yes))))
                      '(sleep)))
 (define exec (make-machine-execution recover-fsm))
 (define in-error (handle-event (execution-start exec) 'wake))
 (check-equal? (machine-execution-condition in-error) 'in-error)
 (hash-set! guard 'say-yes #t)
 (define recovered (complete-current-state in-error))
 (check-equal? (machine-execution-condition recovered) 'completed))

(test-case
 "handle-event: success - recover from indeterminate error"
 (define guard (make-hash (list (cons 'say-yes #t))))
 (define recover-fsm (make-state-machine
                      'first-fsm
                      (list (make-state 'hello 'start)
                            (make-state 'goodbye 'final))
                      (list (make-transition 'hello 'goodbye
                                             #:on-event 'wake)
                            (make-transition 'hello 'goodbye
                                             #:on-event 'wake
                                             #:guard (λ (ex ev) (hash-ref guard 'say-yes))))
                      '(sleep)))
 (define exec (make-machine-execution recover-fsm))
 (define in-error (handle-event (execution-start exec) 'wake))
 (check-equal? (machine-execution-condition in-error) 'in-error)
 (hash-set! guard 'say-yes #f)
 (define recovered (complete-current-state in-error))
 (check-equal? (machine-execution-condition recovered) 'completed))

(test-case
 "complete-current-state: success"
 (define exec (make-machine-execution simple-fsm))
 (define started (execution-start exec))
 (define next (complete-current-state started))
 (check-equal? (machine-execution-condition next) 'completed)
 (check-equal? (state-name (machine-execution-current-state next)) 'goodbye))

;; TODO: check internal transition

(test-case
 "complete-current-state: fail on created"
 (define exec (make-machine-execution blocked-fsm))
 (check-exn exn:fail:contract?
            (λ () (complete-current-state exec))))

(test-case
 "complete-current-state: fail on no transition"
 (define exec (make-machine-execution blocked-fsm))
 (define in-error (complete-current-state (execution-start exec)))
 (check-equal? (machine-execution-condition in-error) 'in-error))

(test-case
 "complete-current-state: fail on multiple transitions"
 (define exec (make-machine-execution indeterminate-fsm))
 (define in-error (complete-current-state (execution-start exec)))
 (check-equal? (machine-execution-condition in-error) 'in-error))

(test-case
 "complete-current-state: success - recover from blocking error"
 (define guard (make-hash (list (cons 'say-yes #f))))
 (define recover-fsm (make-state-machine
                      'first-fsm
                      (list (make-state 'hello 'start)
                            (make-state 'goodbye 'final))
                      (list (make-transition 'hello 'goodbye
                                             #:guard (λ (ex ev) (hash-ref guard 'say-yes))))
                      '(sleep)))
 (define exec (make-machine-execution recover-fsm))
 (define in-error (complete-current-state (execution-start exec)))
 (check-equal? (machine-execution-condition in-error) 'in-error)
 (hash-set! guard 'say-yes #t)
 (define recovered (complete-current-state in-error))
 (check-equal? (machine-execution-condition recovered) 'completed))

(test-case
 "complete-current-state: success - recover from indeterminate error"
 (define guard (make-hash (list (cons 'say-yes #t))))
 (define recover-fsm (make-state-machine
                      'first-fsm
                      (list (make-state 'hello 'start)
                            (make-state 'goodbye 'final))
                      (list (make-transition 'hello 'goodbye)
                            (make-transition 'hello 'goodbye
                                             #:guard (λ (ex ev) (hash-ref guard 'say-yes))))
                      '(sleep)))
 (define exec (make-machine-execution recover-fsm))
 (define in-error (complete-current-state (execution-start exec)))
 (check-equal? (machine-execution-condition in-error) 'in-error)
 (hash-set! guard 'say-yes #f)
 (define recovered (complete-current-state in-error))
 (check-equal? (machine-execution-condition recovered) 'completed))

;; TODO: check reporter