#lang info
;;
;; Behavioral Models.
;;
;; ~ Simon Johnston 2018.
;;

(define collection 'multi)

(define pkg-desc "Various models for describing behavior.")
(define version "1.0")
(define pkg-authors '(johnstonskj))

(define deps '(
  "base"
  "rackunit-lib"
  "racket-index"))
(define build-deps '(
  "scribble-lib"
  "scribble-math"
  "racket-doc"
  "sandbox-lib"
  "cover-coveralls"))
