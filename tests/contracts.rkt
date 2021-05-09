#lang racket/base

(require (for-syntax racket/base)
         chk)

(begin-for-syntax (putenv "RKT_PRIVATE_CONTRACTS" "1"))
(require "../main.rkt")

(: foo (-> integer? integer?))
(define (foo x) x)

(module+ test
  (chk
   #:x (foo "hello")
   "integer?"))
