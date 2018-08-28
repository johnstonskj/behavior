#lang racket
;;
;; Behavioral Models -- Petri Net.
;;
;;
;; ~ Simon Johnston 2018.

;; ---------- Requirements

(require rackunit
         ; ---------
         behavior/petri-net
         behavior/reporter)

;; ---------- Test Fixtures

;; ---------- Internal procedures

;; ---------- Test Cases

(test-case
 "make-petri-net: succes, simple success"
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
 (execute-net exec))