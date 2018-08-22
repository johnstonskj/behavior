#lang racket
;;
;; Shared Testing Utilities
;;
;;
;; ~ Simon Johnston 2018.

(provide
 
 ignore-test-case
 
 test-doc-coverage)

;; ---------- Requirements

(require
  rackunit
  rackunit/docs-complete)

;; ---------- Test Utilities

(define (ignore-test-case name . exprs)
  (displayln (format "TODO: ignoring test '~a'" name)))

(define (test-doc-coverage module-list)
  (for ([module module-list])
    (test-case
     (format "test for documentation in ~a" module)
     (let ([s (open-output-string)])
       (parameterize ([current-error-port s])
         (check-docs module))
       (define out (get-output-string s))
       (when (non-empty-string? out)
         (displayln out))
       (check-eq? (string-length out) 0)))))
