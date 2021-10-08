#lang racket/base

(require chk
         racket/contract
         "../annotate.rkt")

;; set `CONTRACT_EXERCISE_TEST`

(void (putenv "CONTRACT_EXERCISE_TEST" "1"))

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

;; inside `test` submodule

(module+ test
  (: baz-fo integer?)
  (define baz-fo 42)

  (: baz-ho (-> integer?))
  (define (baz-ho) "hi")

  (chk
   baz-fo 42
   #:t (procedure? baz-ho)
   #:x (foo-ho)
   "foo-ho: broke its own contract"
   #:x (baz-ho)
   "baz-ho: broke its own contract"

   #:x
   (let ()
     (: qux-fo integer?)
     (define (qux-fo) 42)
     (void))
   "qux-fo: broke its own contract"

   #:x
   (let ()
     (: qux-ho (-> integer?))
     (define (qux-ho) "hi")
     (qux-ho))
   "qux-ho: broke its own contract"))
