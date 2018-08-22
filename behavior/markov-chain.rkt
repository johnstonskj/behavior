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
   (->* () #:rest (listof pair?) (or/c #f mkchain?))]

  [make-diagonal-chain
   (-> (listof symbol?) mkchain?)]

  [chain-states
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

  [make-execution
   (->* (mkchain? symbol?) (mkchain-reporter?) (or/c #f execution?))]

  [make-execution-generator
   (-> mkchain? symbol? (or/c #f generator?))]

  [execute
   (-> execution? exact-positive-integer? execution?)]

  [execute-next
   (-> execution? execution?)]

  [execution-state
   (-> execution? symbol?)]

  [execution-trace
   (-> execution? (listof symbol?))]

  [execution-complete?
   (-> execution? boolean?)]
  
  [mkchain->graph
   (-> mkchain? port? void?)]

  [mkchain->graph-string
   (-> mkchain? string?)]

  [mkchain? contract?]

  [mkchain-row? contract?]

  [mkchain-reporter? contract?]))

;; ---------- Requirements

(require racket/generator)

;; ---------- Internal types

(struct mkchain
  (matrix))

(struct execution
  (matrix
   [steps #:mutable]
   reporter
   [done #:mutable]))

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

(define (make-chain . chain-row-pairs)
  (define a-chain (mkchain (make-hash chain-row-pairs)))
  (if (for/and ([(state row) (mkchain-matrix a-chain)])
        (row-validate a-chain row))
      a-chain
      #f))

(define (make-diagonal-chain states)
  (mkchain
   (make-hash
    (for/list ([state states])
      (cons state (hash state 1.0))))))

(define (chain-states chain)
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

(define (make-execution-generator chain start-state)
  (define (g-reporter state) (yield state))
  (generator ()
             (define exec (make-full-execution chain start-state g-reporter))
             (when exec
               (let more ([exec (execute-next exec)])
                 (if (execution-done exec)
                     (yield #f)
                     (more (execute-next exec)))))))

(define (make-execution chain start-state [reporter #f])
  (make-full-execution chain start-state reporter))

(define (execute exec steps)
  (when (and (not (execution-done exec)) (> steps 0))
    (execute (execute-next exec) (sub1 steps)))
  exec)
  
(define (execute-next exec)
  (when (not (execution-done exec))
    (define next-step (next (execution-matrix exec) (first (execution-steps exec))))
    (if next-step
        (if (equal? (execution-reporter exec) #f)
            (set-execution-steps!
             exec
             (cons next-step (execution-steps exec)))
            (begin
              (set-execution-steps! exec (list next-step))
              ((execution-reporter exec) next-step)))
        (set-execution-done! exec #t)))
  exec)

(define (execution-trace exec)
  (execution-steps exec))

(define (execution-state exec)
  (first (execution-trace exec)))

(define (execution-complete? exec)
  (execution-done exec))

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
  (if (member start-state (chain-states chain))
      (let ([exec (execution
                   (for/hash ([(row-state row) (mkchain-matrix chain)])
                     (values row-state (probability-ranges row)))
                   (list start-state)
                   reporter
                   #f)])
        (when (and exec reporter)
          (reporter start-state))
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
