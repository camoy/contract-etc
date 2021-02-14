#lang info

;; General

(define collection "contract-etc")
(define pkg-desc "Contract miscellanea.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/contract-etc.scrbl" ())))

;; Dependencies

(define deps
  '("base"))

(define build-deps
  '("chk-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"
    ))
