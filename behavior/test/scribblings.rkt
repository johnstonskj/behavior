#lang racket
;;
;; Behavioral Models.
;;
;;
;; ~ Simon Johnston 2018.

;; ---------- Requirements

(require
  rackunit
  rackunit/docs-complete)

;; ---------- Test Cases

(for ([module '(behavior/markov-chain)])
  (test-case
   (format "test for documentation in ~a" module)
   (let ([s (open-output-string)])
     (parameterize ([current-error-port s])
       (check-docs module))
     (displayln (get-output-string s))
     (check-eq? (string-length (get-output-string s)) 0))))
