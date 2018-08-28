#lang racket
;;
;; Behavioral Models -- Reporter Functions.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [make-port-reporter
   (-> output-port? (-> history-event? void?))]
  
  [make-logging-reporter
   (->* () (logger?) (-> history-event? void?))]
  
  [make-channel-reporter
   (-> (values channel? (-> history-event? void?)))])

 (except-out
  (struct-out history-event)
  private-history-event!))

;; ---------- Requirements

(require)

;; ---------- Internal types

(struct history-event
  (time)
  #:constructor-name private-history-event!)

;; ---------- Implementation

(define (make-port-reporter port)
  (λ (ev) (unless (port-closed? port)
            (displayln ev port))))

(define (make-logging-reporter [a-logger (current-logger)])
  (λ (ev) (log-info (~s ev))))

(define (make-channel-reporter)
  (define reporting-channel (make-channel))
  (values reporting-channel
          (λ (ev) (channel-put reporting-channel ev))))
