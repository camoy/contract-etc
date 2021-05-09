#lang racket/base

(require (for-syntax racket/base)
         chk)

(begin-for-syntax
  (define var
    (string->bytes/locale "RKT_PRIVATE_CONTRACTS" (char->integer #\?)))
  (environment-variables-set! (current-environment-variables) var #f))

(require "../main.rkt")

(: bar (-> integer? integer?))
(define (bar x) x)

(module+ test
  (chk
   (bar "hello")))
