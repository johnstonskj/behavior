#lang racket
;;
;; Behavioral Models -- Markov Chains.
;;
;;
;; ~ Simon Johnston 2018.

(provide
 (contract-out

  [==>
   (->* (symbol?) #:rest (listof pair?) pair?)]

  [==>!
   (-> symbol? pair?)]

  [-->
   (-> symbol? real? pair?)]
  
  [make-chain
   (->* (symbol?) #:rest (listof pair?) (or/c #f mkchain?))]

  [make-diagonal-chain
   (-> symbol? (listof symbol?) mkchain?)]

  [mkchain-states
   (-> mkchain? (listof symbol?))]

  [-->?
   (-> mkchain? symbol? symbol? boolean?)]
  
  [<-->?
   (-> mkchain? symbol? symbol? boolean?)]
  
  [>--<?
   (-> mkchain? symbol? boolean?)]
  
  [row-ref
   (-> mkchain? symbol? mkchain-row?)]

  [row-set
   (-> mkchain? symbol? mkchain-row? (or/c #f mkchain?))]

  [make-chain-execution
   (->* (mkchain? symbol?) ((-> chain-history-event? any/c)) (or/c #f chain-execution?))]

  [make-chain-execution-generator
   (-> mkchain? symbol? (or/c #f generator?))]

  [execute-chain
   (-> chain-execution? exact-positive-integer? chain-execution?)]

  [execute-chain-step
   (-> chain-execution? chain-execution?)]

  [mkchain->graph
   (-> mkchain? port? void?)]

  [mkchain->graph-string
   (-> mkchain? string?)]

  [mkchain-row? contract?])

  (except-out
   (struct-out mkchain)
   private-mkchain
   mkchain-matrix)

  (except-out
   (struct-out chain-execution)
   private-chain-execution
   chain-execution-matrix
   chain-execution-reporter
   set-chain-execution-complete?!
   set-chain-execution-state!)

  (except-out
   (struct-out chain-history-event)
   private-history-event))

;; ---------- Requirements

(require racket/generator behavior/reporter)

;; ---------- Internal types

(struct mkchain
  (name
   matrix)
  #:constructor-name private-mkchain)

(struct chain-execution
  (model
   matrix
   [state #:mutable]
   reporter
   [complete? #:mutable])
  #:constructor-name private-chain-execution)

(struct chain-history-event history-event
  (current-execution
   state)
  #:constructor-name private-history-event
  #:transparent)

;; ---------- Implementation - Chain Definition

(define mkchain-row?
  (hash/c symbol? real?))

(define mkchain-reporter?
  (or/c #f (-> symbol? any/c)))

(define (==>! from-state)
  (cons from-state (hash)))

(define (==> from-state . pairs)
  (cons from-state (make-hash pairs)))

(define (--> to-state probability)
  (cons to-state probability))

(define (make-chain name . chain-row-pairs)
  (define a-chain (private-mkchain name (make-hash chain-row-pairs)))
  (if (for/and ([(state row) (mkchain-matrix a-chain)])
        (row-validate a-chain row))
      a-chain
      #f))

(define (make-diagonal-chain name states)
  (private-mkchain
   name
   (make-hash
    (for/list ([state states])
      (cons state (hash state 1.0))))))

(define (mkchain-states chain)
  (hash-keys (mkchain-matrix chain)))

(define (-->? chain from-state to-state)
  (> (transition-probability chain from-state to-state) 0))

(define (<-->? chain state1 state2)
  (and (-->? chain state1 state2)
       (-->? chain state2 state1)))

(define (>--<? chain state)
  (or
   (= (transition-probability chain state state) 1.0)
   (= (for/sum ([(s probability) (row-ref chain state)])
        probability)
      0.0)))

(define (row-ref chain state)
  (hash-ref (mkchain-matrix chain)  state))

(define (row-set chain state row)
  (if (row-validate chain row)
      (begin
        (hash-set! (mkchain-matrix chain) state row)
        chain)
      #f))

;; ---------- Implementation - Chain Execution

(define (make-chain-execution-generator chain start-state)
  (define (g-reporter ev) (yield (chain-history-event-state ev)))
  (generator ()
             (define exec (make-full-execution chain start-state g-reporter))
             (when exec
               (let more ([exec (execute-chain-step exec)])
                 (if (chain-execution-complete? exec)
                     (yield #f)
                     (more (execute-chain-step exec)))))))

(define (make-chain-execution chain start-state [reporter #f])
  (make-full-execution chain start-state reporter))

(define (execute-chain exec steps)
  (when (and (not (chain-execution-complete? exec)) (> steps 0))
    (execute-chain (execute-chain-step exec) (sub1 steps)))
  exec)
  
(define (execute-chain-step exec)
  (when (not (chain-execution-complete? exec))
    (define next-step (next (chain-execution-matrix exec) (chain-execution-state exec)))
    (if next-step
        (begin
          (set-chain-execution-state! exec next-step)
          (when (chain-execution-reporter exec)
            ((chain-execution-reporter exec)
             (private-history-event
              (current-inexact-milliseconds)
              exec
              next-step))))
        (set-chain-execution-complete?! exec #t)))
  exec)

;; ---------- Implementation - Chain Visualization

(define (mkchain->graph chain port)
  (displayln "digraph markov_chain {" port)
  (displayln "    rankdir = LR;" port)
  (displayln "    size = \"8,5\";" port)
  (displayln "    node [shape = circle];" port)
  (for ([(row-symbol row-hash) (mkchain-matrix chain)])
    (for ([(col-symbol col-probability) row-hash])
      (displayln (format "    ~s -> ~s [label = \"~s\"];"
                         row-symbol col-symbol col-probability)
                 port)))
  (displayln "}" port))
  
(define (mkchain->graph-string chain)
  (define port (open-output-string))
  (mkchain->graph chain port)
  (get-output-string port))

;; ---------- Internal procedures

(define (make-full-execution chain start-state reporter)
  (if (member start-state (mkchain-states chain))
      (let ([exec (private-chain-execution
                   (mkchain-name chain)
                   (for/hash ([(row-state row) (mkchain-matrix chain)])
                     (values row-state (probability-ranges row)))
                   start-state
                   (if reporter reporter (make-null-reporter))
                   #f)])
        ((chain-execution-reporter exec)
         (private-history-event (current-inexact-milliseconds)
                                exec
                                start-state))
        exec)
      #f))

(define (next exec-chain state)
  (find-next-state
   (hash-ref exec-chain state)
   (random)))

(define (find-next-state row value [last 0])
  (cond
    [(equal? row '()) #f]
    [(and (> value last)
          (<= value (car (first row))))
     (cdr (first row))]
    [else (find-next-state (rest row) value (car (first row)))]))

(define (transition-probability chain from-state to-state)
  (hash-ref
   (row-ref chain from-state)
   to-state
   0))

(define sum-start (cons 0 #f))

(define (probability-ranges row-hash)
  (remove sum-start
          (reverse
           (for/fold ([ranges (list sum-start)])
                     ([(state probability) row-hash])
             (cons (cons (+ (car (first ranges)) probability) state) ranges)))))

(define (row-validate chain row)
  (and
   (for/and ([col (hash-keys row)])
     (hash-has-key? (mkchain-matrix chain) col))
   (let ([sum (for/sum ([(state probability) row]) probability)])
     (or (= sum 0.0) (= sum 1.0)))))
