#lang racket/base

(require chk)

;; waive

(module foo racket/base
  (require racket/contract
	   contract-etc
	   contract-etc/annotate)

  (provide (waive-out hello))

  (: hello (-> integer?))
  (define (hello) "42"))

;; exercise

(module bar racket/base
  (require racket/contract
	   contract-etc
	   contract-etc/annotate)

  (provide (exercise-out hi))

  (: hi (-> integer?))
  (define (hi) "42"))

;; tests

(require 'foo 'bar)
(chk
  (hello)  "42"
  #:x (hi)
  "hi: broke its own contract"
  )
