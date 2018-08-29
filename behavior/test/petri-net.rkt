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

;; ---------- Test Cases - Definition

(test-case
 "make-petri-net: success, simple net defined and executed"
 (define net (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'a 't 1)
                                  (make-arc 't 'b 1))))
 (define expected '((b consumes 1)(t firing 0)(a emits 1)))
 (let-values ([(buffer reporter) (make-buffering-reporter)])
   (define exec (make-net-execution net (hash 'a 1) reporter))
   (execute-net exec)
   (check-equal? (length (buffer)) (length expected))
   (for ([event (buffer)] [value expected])
     (check-equal? (net-history-event-place-or-transition event)
                   (first value))
     (check-equal? (net-history-event-kind event)
                   (second value))
     (check-equal? (length (net-history-event-tokens event))
                   (third value)))))

(test-case
 "make-petri-net: success - X network"
 (check-true (petri-net? 
              (make-petri-net 'first-net
                              (set 'a 'b 'x 'y)
                              (set 't)
                              (set (make-arc 'a 't 1)
                                   (make-arc 'b 't 1)
                                   (make-arc 't 'x 1)
                                   (make-arc 't 'y 1))))))

(test-case
 "make-petri-net: success - cycle"
 (check-true (petri-net? 
              (make-petri-net 'first-net
                              (set 'P1 'P2 'P3 'P4)
                              (set 'T1 'T2)
                              (set (make-arc 'P1 'T1 1)
                                   (make-arc 'T1 'P2 1)
                                   (make-arc 'T1 'P3 1)
                                   (make-arc 'P2 'T2 1)
                                   (make-arc 'P3 'T2 1)
                                   (make-arc 'T2 'P4 1)
                                   (make-arc 'T2 'P1 1))))))

(test-case
 "make-petri-net: failure on overlapping place/transition names"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 'a)
                             (set (make-arc 'a 't 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on empty places"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set)
                             (set 't)
                             (set (make-arc 'a 't 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on empty transitions"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set)
                             (set (make-arc 'a 't 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on empty arcs"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set)))))

(test-case
 "make-petri-net: failure on place/place arc"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'a 'b 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on transition/transition arc"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 't 't 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on inhibiting arc"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'a 't 0)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on bad arc source"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'c 't 1)
                                  (make-arc 't 'b 1))))))

(test-case
 "make-petri-net: failure on bad arc target"
  (check-exn exn:fail:contract?
            (λ ()
              (make-petri-net 'first-net
                             (set 'a 'b)
                             (set 't)
                             (set (make-arc 'a 'x 1)
                                  (make-arc 't 'b 1))))))

;; ---------- Test Cases - Execution

(test-case
 "make-net-execution: success"
 (define net (make-petri-net 'first-net
                             (set 'P1 'P2 'P3 'P4)
                             (set 'T1 'T2)
                             (set (make-arc 'P1 'T1 1)
                                  (make-arc 'T1 'P2 1)
                                  (make-arc 'T1 'P3 1)
                                  (make-arc 'P2 'T2 1)
                                  (make-arc 'P3 'T2 1)
                                  (make-arc 'T2 'P4 1)
                                  (make-arc 'T2 'P1 1))))
 (define exec (make-net-execution net (hash 'P1 1 'P3 2 'P4 1)))
 (check-equal? (net-execution-model exec) net)
 (check-equal? (hash-count (net-execution-place-tokens exec)) 4)
 (for ([expected '((P1 1)(P2 0)(P3 2)(P4 1))])
   (check-equal? (length
                  (hash-ref (net-execution-place-tokens exec) (first expected)))
                 (second expected))))

(test-case
 "make-net-execution: fail on bad configuration"
 (define net (make-petri-net 'first-net
                             (set 'P1 'P2 'P3 'P4)
                             (set 'T1 'T2)
                             (set (make-arc 'P1 'T1 1)
                                  (make-arc 'T1 'P2 1)
                                  (make-arc 'T1 'P3 1)
                                  (make-arc 'P2 'T2 1)
                                  (make-arc 'P3 'T2 1)
                                  (make-arc 'T2 'P4 1)
                                  (make-arc 'T2 'P1 1))))
 (check-exn exn:fail:contract?
            (λ ()
              (make-net-execution net (hash 'P0 1)))))


(test-case
 "make-net-execution: missing configuration"
 (define net (make-petri-net 'first-net
                             (set 'P1 'P2 'P3 'P4)
                             (set 'T1 'T2)
                             (set (make-arc 'P1 'T1 1)
                                  (make-arc 'T1 'P2 1)
                                  (make-arc 'T1 'P3 1)
                                  (make-arc 'P2 'T2 1)
                                  (make-arc 'P3 'T2 1)
                                  (make-arc 'T2 'P4 1)
                                  (make-arc 'T2 'P1 1))))
 (check-exn exn:fail:contract?
            (λ ()
              (make-net-execution net (hash)))))


(test-case
 "make-net-execution: fail on zeroed configuration"
 (define net (make-petri-net 'first-net
                             (set 'P1 'P2 'P3 'P4)
                             (set 'T1 'T2)
                             (set (make-arc 'P1 'T1 1)
                                  (make-arc 'T1 'P2 1)
                                  (make-arc 'T1 'P3 1)
                                  (make-arc 'P2 'T2 1)
                                  (make-arc 'P3 'T2 1)
                                  (make-arc 'T2 'P4 1)
                                  (make-arc 'T2 'P1 1))))
 (check-exn exn:fail:contract?
            (λ ()
              (make-net-execution net (hash 'P1 0)))))

(test-case
 "execute-net-step: success"
 (define net (make-petri-net 'first-net
                             (set 'P1 'P2 'P3 'P4)
                             (set 'T1 'T2)
                             (set (make-arc 'P1 'T1 1)
                                  (make-arc 'T1 'P2 1)
                                  (make-arc 'T1 'P3 1)
                                  (make-arc 'P2 'T2 1)
                                  (make-arc 'P3 'T2 1)
                                  (make-arc 'T2 'P4 1)
                                  (make-arc 'T2 'P1 1))))
 (define exec (make-net-execution net (hash 'P1 1 'P3 2 'P4 1)))
 (execute-net-step exec)
 (for ([expected '((P1 0)(P2 1)(P3 3)(P4 1))])
   (check-equal? (length
                  (hash-ref (net-execution-place-tokens exec) (first expected)))
                 (second expected)))
 (execute-net-step exec)
  (for ([expected '((P1 1)(P2 0)(P3 2)(P4 2))])
   (check-equal? (length
                  (hash-ref (net-execution-place-tokens exec) (first expected)))
                 (second expected))))

