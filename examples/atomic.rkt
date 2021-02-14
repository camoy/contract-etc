#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         contract-etc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Prevents the argument function from being invoked.
(define atomic/c
  (-> (apply/c [none/c #t]) any))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  ;; Constant function returns 42.
  (define/contract (atomic f)
    atomic/c
    42)

  ;; Calls the given argument with 42 and then returns 42.
  (define/contract (non-atomic f)
    atomic/c
    (f 42)
    42)

  (chk
   (atomic (λ _ 42))
   42

   #:x
   (non-atomic (λ _ 42))
   "none/c allows no values"
   ))
