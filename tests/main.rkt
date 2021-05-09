#lang racket/base

(require chk
         "no-contracts.rkt"
         "contracts.rkt")

(module+ test
  (chk
   #:x
   (foo "hello")
   "integer?"
   (bar "hello")))
