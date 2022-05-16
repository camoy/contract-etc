#lang info

;; General

(define collection "contract-etc")
(define pkg-desc "Miscellaneous contracts.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/contract-etc.scrbl" ())))

;; Dependencies

(define deps
  '("rackunit-lib"
    "option-contract-lib"
    "base"))

(define build-deps
  '("option-contract-doc"
    "sandbox-lib"
    "chk-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"
    ))
