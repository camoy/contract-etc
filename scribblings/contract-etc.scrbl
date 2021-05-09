#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/contract
                    contract-etc]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/contract
               racket/function
               racket/match
               contract-etc)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Contract Miscellanea}
@author{Cameron Moy}

@defmodule[contract-etc]

@margin-note{
This library is experimental;
compatibility may not be maintained.
}

@defproc[(dynamic->d [make-contract (-> (unconstrained-domain-> contract?)
                                        contract?)])
          contract?]{
  This contract protected a procedure.
  It applies @racket[make-contract]
  when the procedure is called with its arguments.
  The return value is expected to be a function contract
  that is then applied to the procedure,
  and the arguments are then reapplied to that protected procedure.
}

@examples[#:eval evaluator
  (define increasing/c
    (dynamic->d
      (λ (x)
        (-> integer? (>/c x)))))
  (define/contract add1* increasing/c add1)
  (add1* 42)
  (define/contract values* increasing/c values)
  (eval:error (values* 42))]

@defproc[(self/c [make-contract (-> any/c contract?)]
                 [#:chaperone? chaperone? boolean? #f])
         contract?]{
  Constructs a contract where the contract itself
  depends on the value it's protecting.
  When the contract is attached to a value,
  @racket[make-contract] is applied to it
  and the resulting contract is then attached to that value.
}

@examples[#:eval evaluator
  (define cdr-returns-car/c
    (self/c
      (λ (p)
        (match-define (cons x f) p)
        (cons/c any/c (-> x)))))
  (define/contract good-self cdr-returns-car/c (cons 1 (const 1)))
  ((cdr good-self)) 1
  (define/contract bad-self cdr-returns-car/c (cons 1 (const 2)))
  (eval:error ((cdr bad-self)))]

@deftogether[
  (@defform[
    (apply/c [contract-expr to-protect-expr maybe-positive] ...+)
    #:grammar
    [(maybe-positive (code:line)
                     (code:line #:positive))]]
   @defform[
    (return/c [contract-expr to-protect-expr maybe-positive] ...+)
    #:grammar
    [(maybe-positive (code:line)
                     (code:line #:positive))]])]{
  These contracts expect a procedure
  and sends a constant value through another contract
  when the procedure is applied or returns respectively.
  The @racket[#:positive] option swaps the blame for
  violation of the contract.
}

@examples[#:eval evaluator
  (define (apply-at-most-once/c)
    (define count 0)
    (define (incr n)
      (set! count (+ count n))
      (<= count 1))
    (apply/c [incr 1]))
  (define/contract f (apply-at-most-once/c) void)
  (f)
  (eval:error (f))]

@defform[(: id contract-expr)]{
  If the @indexed-envvar{RKT_PRIVATE_CONTRACTS} environment variable
  is set at compile time,
  then this annotation provides @racket[id] with the given contract.
  Additionally,
  the references to @racket[id] within the test submodule
  are contracted.
  If the environment variable isn't set,
  then @racket[id] is provided without any contracts.
}
