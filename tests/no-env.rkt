#lang racket/base

(require chk
         racket/contract
         "../annotate.rkt")

;; outside `test` submodule

(: foo-fo integer?)
(define foo-fo 42)

(: foo-ho (-> integer?))
(define (foo-ho) "hi")

(chk
 foo-fo 42
 #:t (procedure? foo-ho)
 (foo-ho) "hi"

 #:x
 (let ()
   (: bar-fo integer?)
   (define (bar-fo) 42)
   (void))
 "bar-fo: broke its own contract"

 (let ()
   (: bar-ho (-> integer?))
   (define (bar-ho) "hi")
   (bar-ho))
 "hi")

;; make sure we don't break submodule order
(module+ examples
  (provide (all-defined-out))
  (define forty-two 42))

;; inside `test` submodule

(module+ test
  (require (submod ".." examples))

  (: baz-fo integer?)
  (define baz-fo forty-two)

  (: baz-ho (-> integer?))
  (define (baz-ho) "hi")

  (chk
   baz-fo 42
   #:t (procedure? baz-ho)
   (baz-ho) "hi"
   (foo-ho) "hi"

   #:x
   (let ()
     (: qux-fo integer?)
     (define (qux-fo) 42)
     (void))
   "qux-fo: broke its own contract"

   (let ()
     (: qux-ho (-> integer?))
     (define (qux-ho) "hi")
     (qux-ho))
   "hi"))
