#lang racket
;;
;; Behavioral Models -- Petri Net.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [make-petri-net
   (->* (symbol? (set/c symbol?) (set/c symbol?) (set/c arc?))
        (#:inhibitors? boolean?)
        petri-net?)]
  
;  [make-colored-petri-net
;   (->* (symbol? (hash/c symbol? symbol?) (set/c symbol?) (set/c arc?))
;        (#:inhibitors? boolean?)
;        petri-net?)]

  [make-arc
   (-> symbol? symbol? exact-nonnegative-integer? arc?)]

;  [make-colored-arc
;   (-> symbol? symbol? exact-nonnegative-integer?
;  (-> arc? (set/c arc?) (values boolean? (listof symbol?))) arc?)]

  [petri-net-place-set
   (-> petri-net? (set/c symbol?))]

  [petri-net-transition-set
   (-> petri-net? (set/c symbol?))]
   
  [make-net-execution
   (->* (petri-net? (hash/c symbol? (or/c exact-nonnegative-integer? (listof symbol?))))
        ((-> net-history-event? void?)) net-execution?)]

  [execute-net
   (-> net-execution? net-execution?)]

  [execute-net-step
   (-> net-execution? net-execution?)]

  [net-execution-complete?
   (-> net-execution? boolean?)]

  [token symbol?]

  [tokens
   (-> exact-positive-integer? (listof symbol?))])

  (except-out
   (struct-out petri-net)
   private-petri-net
   petri-net-places
   petri-net-transitions
   petri-net-arcs-from
   petri-net-arcs-to)

  (except-out
   (struct-out arc)
   private-arc
   arc-expression)

  (except-out
   (struct-out net-execution)
   private-net-execution
   net-execution-reporter)

  (except-out
   (struct-out net-history-event)
   private-net-history-event))

;; ---------- Requirements

(require behavior/reporter)

;; ---------- Internal types

(struct petri-net
  (name
   colored?
   places       ; (or/c (listof symbol?) (hash/c symbol? symbol?))
   transitions  ; (listof symbol?)
   arc-set      ; (set/c arc)
   arcs-from    ; (hash/c symbol? arc)
   arcs-to)     ; (hash/c symbol? arc)
  #:constructor-name private-petri-net) 

(struct arc
  (source
   target
   multiplicity ; 0 = inhibitor
   expression)
  #:constructor-name private-arc)

(struct net-execution
  (model
   place-tokens ; (hash/c symbol (listof symbol?))
   reporter)
  #:constructor-name private-net-execution)

(struct net-history-event history-event
  (current-execution
   kind
   place-or-transition
   tokens)
  #:constructor-name private-net-history-event
  #:transparent)

(define token (gensym))

(define (tokens count)
  (make-list count token))

;; ---------- Implementation - Definition

(define (make-petri-net name places transitions arcs #:inhibitors? [inhibitors? #f])
  (validate-net 'make-petri-net #f name places transitions arcs inhibitors?))

(define (make-colored-petri-net name places transitions arcs #:inhibitors? [inhibitors? #f])
  (validate-net 'make-petri-net #t name places transitions arcs inhibitors?))

(define (make-arc from to threshold)
  (private-arc from to threshold #f))

(define (make-colored-arc from to threshold expression)
  (private-arc from to threshold expression))

(define (petri-net-place-set net)
  (if (petri-net-colored? net)
      (hash-keys (petri-net-places net))
      (list->set (petri-net-places net))))

(define (petri-net-transition-set net)
  (list->set (petri-net-transitions)))

;; ---------- Implementation - Execution

(define (make-net-execution net configuration [reporter #f])
  (when (= (hash-count configuration) 0)
    (raise-argument-error 'make-net-execution
                          "initial configuration may not be empty"
                          configuration))
  (unless (for/and ([(p v) configuration]) (or (> v 0) (> (length v) 0)))
    (raise-argument-error 'make-net-execution
                          "initial configuration may not be (effectively) empty"
                          configuration))

  (define initial-configuration (hash-copy (for/hash ([place (place-names net)])
                                             (values place '()))))
  (for ([(place value) configuration])
    (if (hash-has-key? initial-configuration place)
        (cond
          [(list? value)
           (hash-set! initial-configuration place value)]
          [(exact-nonnegative-integer? value)
           (hash-set! initial-configuration place (make-list value token))])
        (raise-argument-error 'make-net-execution
                              "Specified place name is not a valid place in this net"
                              place)))
  (private-net-execution net
                         initial-configuration
                         (if reporter reporter (make-null-reporter))))

(define (execute-net exec)
  (let next ([enabled (enabled-transitions exec)])
    (when (> (length enabled) 0)
      (for ([transition enabled])
        (fire-transition exec transition))
      (next (enabled-transitions exec))))
  exec)

(define (execute-net-step exec)
  (let ([enabled (enabled-transitions exec)])
    (for ([transition enabled])
      (fire-transition exec transition)))
  exec)

(define (net-execution-complete? exec)
  (equal? (enabled-transitions exec) '()))

;; ---------- Internal procedures

(define (place-names net)
  (if (petri-net-colored? net)
      (hash-keys (petri-net-places net))
      (petri-net-places net)))

(define (enabled? exec transition)
  (for/and ([arc (hash-ref (petri-net-arcs-to (net-execution-model exec)) transition)])
    (if (= (arc-multiplicity arc) 0)
        #f ; inhibitor
        (let ([token-count (length (hash-ref (net-execution-place-tokens exec)
                                             (arc-source arc)))])
          (>= token-count (arc-multiplicity arc))))))

(define (enabled-transitions exec)
  (shuffle (filter (λ (t) (enabled? exec t))
                   (petri-net-transitions (net-execution-model exec)))))

(define (fire-transition exec transition)
  (for ([arc (hash-ref (petri-net-arcs-to (net-execution-model exec))
                            transition)])
    (define tokens (hash-ref (net-execution-place-tokens exec) (arc-source arc)))
    (hash-set! (net-execution-place-tokens exec)
               (arc-source arc)
               (list-tail tokens (arc-multiplicity arc)))
    (report-place-emits exec (arc-source arc) (take tokens (arc-multiplicity arc))))
  (report-transition-firing exec transition)
  (for ([arc (hash-ref (petri-net-arcs-from (net-execution-model exec)) transition)])
    (define tokens (hash-ref (net-execution-place-tokens exec) (arc-target arc)))
    (hash-set! (net-execution-place-tokens exec)
               (arc-target arc)
               (append tokens (make-list (arc-multiplicity arc) token)))
    (report-place-consumes exec (arc-target arc) (make-list (arc-multiplicity arc) token))))


(define (evaluate-colored-transition exec transition arc)
  ;; TODO: run evaluations!
  ;; (-> arc? (set/c arc?) (values boolean? (listof symbol?)))
  #f)

(define (validate-net who colored? name places transitions arcs inhibitors?)
  (unless (= (set-count (set-intersect places transitions)) 0)
    (raise-argument-error who
                          "Place and transition names must be distinct"
                          (set-intersect places transitions)))
  (define arc-list (set->list arcs))
  (when (false? inhibitors?)
    (begin
      (define inhibited (filter (λ (arc) (= (arc-multiplicity arc) 0)) arc-list))
      (when (> (length inhibited) 0)
        (raise-argument-error who
                              "Inhibited arcs not allowed without #:inhibitors #t"
                              (set-intersect places transitions)))))
  (define place-names (if colored? (hash-keys places) places))
  (define bad-arcs (filter (λ (arc) (not (xor (set-member? place-names (arc-source arc))
                                              (set-member? place-names (arc-target arc)))))
                           arc-list))
  (when (> (length bad-arcs) 0)
    (raise-argument-error who
                          "Arc from/to must not be between place/place or transition/transition"
                          bad-arcs))
  (private-petri-net name
                     colored?
                     ;; ignores colored? hash
                     (set->list places)
                     (set->list transitions)
                     arcs
                     ;; TODO: below is incomplete, ignores multiple arcs to/from same place
                     (for/hash ([arc arcs]) (values (arc-source arc) (list arc)))
                     (for/hash ([arc arcs]) (values (arc-target arc) (list arc)))))

(define (report-place-emits exec place tokens)
  (when (net-execution-reporter exec)
    ((net-execution-reporter exec)
     (private-net-history-event (current-inexact-milliseconds) exec 'emits place tokens))))

(define (report-transition-firing exec transition)
  (when (net-execution-reporter exec)
    ((net-execution-reporter exec)
     (private-net-history-event (current-inexact-milliseconds) exec 'firing transition '()))))

(define (report-place-consumes exec place tokens)
  (when (net-execution-reporter exec)
    ((net-execution-reporter exec)
     (private-net-history-event (current-inexact-milliseconds) exec 'consumes place tokens))))
