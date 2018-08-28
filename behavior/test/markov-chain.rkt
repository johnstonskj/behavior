#lang racket
;;
;; Behavioral Models -- Markov Chains.
;;
;;
;; ~ Simon Johnston 2018.

;; ---------- Requirements

(require rackunit
         ; ---------
         behavior/markov-chain
         "utilities.rkt")

;; ---------- Test Fixtures

(define a-chain (make-chain 
                 (==> 'a (--> 'a .5) (--> 'b .25) (--> 'c .25))
                 (==> 'b (--> 'c .5) (--> 'd .5))
                 (==> 'c (--> 'a .25) (--> 'b .25) (--> 'c .5))
                 (==>! 'd)))

(define an-exec (make-chain-execution a-chain 'b))

;; ---------- Test Cases - Chain Definition

(test-case
 "make-chain: success"
 (check-equal? (sort (mkchain-states a-chain) symbol<?) '(a b c d))
 (define a-row (row-ref a-chain 'a))
 (check-equal? (hash-count a-row) 3)
 (check-equal? (hash-ref a-row 'a) .5)
 (check-equal? (hash-ref a-row 'b) .25)
 (check-equal? (hash-ref a-row 'c) .25)
 (define b-row (row-ref a-chain 'b))
 (check-equal? (hash-count b-row) 2)
 (check-equal? (hash-ref b-row 'c) .5)
 (check-equal? (hash-ref b-row 'd) .5)
 (define c-row (row-ref a-chain 'c))
 (check-equal? (hash-count c-row) 3)
 (check-equal? (hash-ref c-row 'a) .25)
 (check-equal? (hash-ref c-row 'b) .25)
 (check-equal? (hash-ref c-row 'c) .5)
 (define d-row (row-ref a-chain 'd))
 (check-equal? (hash-count d-row) 0))

(test-case
 "make-chain: failure (bad graph)"
 (check-false (make-chain (==> 'a (--> 'c 1.0)))))

(test-case
 "make-chain: failure (bad probabilities)"
 (check-false (make-chain (==> 'a (--> 'a 1.5))))
 (check-false (make-chain (==> 'a (--> 'a .75) (--> 'b .75))
                          (==>! 'b))))

(test-case
 "make-diagonal-chain: success"
 (define states '(a b c d))
 (define diagonal (make-diagonal-chain states))
 (check-equal? (sort (mkchain-states a-chain) symbol<?) '(a b c d))
 (for ([state states])
   (define a-row (row-ref diagonal state))
   (check-equal? (hash-count a-row) 1)
   (check-equal? (hash-ref a-row state) 1.0)))

(test-case
 "<-->?: success"
 (check-true (<-->? a-chain 'a 'c))
 (check-false (<-->? a-chain 'a 'b))
 (check-false (<-->? a-chain 'a 'd))
 (check-false (<-->? a-chain 'b 'd))
 (check-true (<-->? a-chain 'b 'c))
 (check-false (<-->? a-chain 'c 'd)))

(test-case
 "-->?: success"
 (check-true (-->? a-chain 'a 'a))
 (check-true (-->? a-chain 'a 'b))
 (check-true (-->? a-chain 'a 'c))
 (check-false (-->? a-chain 'a 'd))
 (check-false (-->? a-chain 'b 'a))
 (check-false (-->? a-chain 'b 'b))
 (check-true (-->? a-chain 'b 'c))
 (check-true (-->? a-chain 'b 'd))
 (check-true (-->? a-chain 'c 'a))
 (check-true (-->? a-chain 'c 'b))
 (check-true (-->? a-chain 'c 'c))
 (check-false (-->? a-chain 'c 'd))
 (check-false (-->? a-chain 'd 'a))
 (check-false (-->? a-chain 'd 'b))
 (check-false (-->? a-chain 'd 'c))
 (check-false (-->? a-chain 'd 'd)))

(test-case
 ">--<?: success"
 (check-false (>--<? a-chain 'a))
 (check-false (>--<? a-chain 'b))
 (check-false (>--<? a-chain 'c))
 (check-true (>--<? a-chain 'd)))

(test-case
 "row-set: success"
 (define chain (make-chain
                (==> 'a (--> 'b 1.0))
                (==> 'b (--> 'c 1.0))
                (==>! 'c)))
 (define row (row-ref chain 'b))
 (hash-set! row 'c 0.5)
 (hash-set! row 'a 0.5)
 (check-not-false (row-set chain 'b row)))

(test-case
 "row-set: failure (bad state)"
 (define chain (make-chain
                (==> 'a (--> 'b 1.0))
                (==> 'b (--> 'c 1.0))
                (==>! 'c)))
 (define row (row-ref chain 'b))
 (hash-set! row 'c 0.5)
 (hash-set! row 'd 0.5)
 (check-false (row-set chain 'b row)))

(test-case
 "row-set: failure (bad probabilities)"
 (define chain (make-chain
                (==> 'a (--> 'b 1.0))
                (==> 'b (--> 'c 1.0))
                (==>! 'c)))
 (define row (row-ref chain 'b))
 (hash-set! row 'c 0.75)
 (hash-set! row 'a 0.75)
 (check-false (row-set chain 'b row)))

;; ---------- Test Cases - Chain Execution

(test-case
 "make-chain-execution: success"
 (check-false (execution-chain-complete? an-exec))
 (check-equal? (execution-chain-trace an-exec) '(b))
 (define new-exec (execute-chain an-exec 10))
 (check-true (or
              (= (length (execution-chain-trace an-exec)) 11)
              (equal? (first (execution-chain-trace an-exec)) 'd))))

(test-case
 "make-chain-execution: failure (bad start state)"
 (check-false (make-chain-execution a-chain 'x)))

(test-case
 "make-chain-execution: success (with reporter)"
 (define d-chain (make-chain
                  (==> 'a (--> 'b 1.0))
                  (==> 'b (--> 'c 1.0))
                  (==>! 'c)))
 (define out (open-output-string))
 (define exec (make-chain-execution d-chain 'a (curryr display out)))
 (check-not-false exec)
 (execute-chain exec 10)
 (check-equal? (get-output-string out) "abc"))

(test-case
 "make-chain-execution-generator: success"
 (define d-chain (make-chain
                  (==> 'a (--> 'b 1.0))
                  (==> 'b (--> 'c 1.0))
                  (==>! 'c)))
 (define next (make-chain-execution-generator d-chain 'a))
 (for ([state (in-producer next #f)]
       [expected '(a b c)])
   (check-equal? state expected)))

(test-case
 "execute: success (deterministic)"
 (define d-chain (make-chain
                  (==> 'a (--> 'b 1.0))
                  (==> 'b (--> 'c 1.0))
                  (==>! 'c)))
 (define d-exec (make-chain-execution d-chain 'a))
 (check-equal? (execution-chain-state d-exec) 'a)
 (define new-exec (execute-chain d-exec 10))
 (check-equal? (execution-chain-state new-exec) 'c)
 (check-true (execution-chain-complete? new-exec))
 (check-equal? (execution-chain-trace new-exec) '(c b a)))

;; ---------- Test Cases - Chain Visualization

(test-case
 "mkchain->graph-string: success"
 (define chain (make-chain
                (==> 'a (--> 'b 1.0))
                (==> 'b (--> 'c 1.0))
                (==>! 'c)))
 (define dot (mkchain->graph-string chain))
 (check-true (string-prefix? dot "digraph markov_chain {\n"))
 (check-true (string-contains? dot "    a -> b [label = \"1.0\"];\n"))
 (check-true (string-contains? dot "    b -> c [label = \"1.0\"];\n"))
 (check-true (string-suffix? dot "}\n")))