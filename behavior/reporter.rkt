#lang racket
;;
;; Behavioral Models -- Reporter Functions.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [make-null-reporter
   (-> (-> history-event? any/c))]
  
  [make-port-reporter
   (-> output-port? (-> history-event? any/c))]
  
  [make-logging-reporter
   (->* () (logger?) (-> history-event? any/c))]

  [make-buffering-reporter
   (->* ()
        ((-> history-event? any/c))
        (values (-> list?)
                (-> history-event? any/c)))]
  
  [make-channel-reporter
   (-> (values channel? (-> history-event? any/c)))])

 (except-out
  (struct-out history-event)
  private-history-event!))

;; ---------- Internal types

(struct history-event
  (time)
  #:constructor-name private-history-event!)

;; ---------- Implementation

(define (make-null-reporter)
  (λ (ev) #f))
  
(define (make-port-reporter port)
  (λ (ev) (unless (port-closed? port)
            (displayln ev port))))

(define (make-logging-reporter [a-logger (current-logger)])
  (λ (ev) (log-info (~s ev))))

(define (make-buffering-reporter [selector identity])
  (define trace (make-hash (list (cons 'buffer '()))))
  (values
   (λ () (hash-ref trace 'buffer))
   (λ (ev) (hash-set! trace
                      'buffer
                      (cons (selector ev)
                            (hash-ref trace 'buffer))))))

(define (make-channel-reporter)
  (define reporting-channel (make-channel))
  (values reporting-channel
          (λ (ev) (channel-put reporting-channel ev))))
