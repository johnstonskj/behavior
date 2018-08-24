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

;; ---------- Internal procedures

(define (check-log-sequence log-string expected-states)
  (define log-list (string-split (get-output-string log-string) "\n" #:repeat? #t))
  (check-equal? (length log-list) (length expected-states))
  (for ([log-line log-list]
        [state expected-states])
    (check-equal? (fourth (string-split log-line " " #:repeat? #t))
                  (symbol->string state))))
  
;; ---------- Test Cases

(test-case
 "simple state machine: success"
 (define expected '(starting enter-state execute-state handle-event handle-event
                             transition enter-state execute-state completed))
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