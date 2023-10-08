#lang info

;; General

(define collection "contract-etc")
(define pkg-desc "Miscellaneous contracts.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/contract-etc.scrbl" ())))

;; Dependencies

(define deps
  '("contract-etc-lib"
    "base"))

(define build-deps
  '("chk-lib"
    "option-contract-doc"
    "option-contract-lib"
    "racket-doc"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))
