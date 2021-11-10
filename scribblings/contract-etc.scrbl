#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/class
                    racket/contract
                    racket/contract/option
                    racket/function
                    racket/list
                    racket/match
                    racket/string
                    contract-etc
                    (only-in contract-etc/annotate :)]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/class
               racket/contract
               racket/contract/option
               racket/function
               racket/list
               racket/match
               racket/string
               contract-etc
               contract-etc/annotate)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Contract Miscellanea}
@author{Cameron Moy}

@defmodule[contract-etc]

@section{Combinators}

@margin-note{
This library is experimental;
compatibility may not be maintained.
}

@defproc[(dynamic->d [make-contract (-> (unconstrained-domain-> contract?)
                                        contract?)])
          contract?]{
  This contract protects a procedure.
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
    (apply/c [contract-expr to-protect-expr maybe-swap] ...+)
    #:grammar
    [(maybe-swap (code:line)
                 (code:line #:swap))]]
   @defform[
    (return/c [contract-expr to-protect-expr maybe-swap] ...+)
    #:grammar
    [(maybe-swap (code:line)
                 (code:line #:swap))]])]{
  These contracts expect a procedure
  and sends a constant value through another contract
  when the procedure is applied or returns respectively.
  The @racket[#:swap] option swaps the blame for
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

@defproc[(class-object/c [class-contract contract?]
                         [object-contract contract?])
          contract?]{
  Creates a class contract that acts exactly like @racket[class-contract],
  except that instantiated objects are additionally constrained by
  @racket[object-contract].
}

@examples[#:eval evaluator
  (define cat%/c
    (class-object/c
      (class/c [meow (->m integer? string?)])
      (object/c [meow (->m positive? string?)])))
  (define/contract cat%
    cat%/c
    (class object%
      (define/public (meow n)
        (string-join (map (const "meow") (range n))))
      (super-new)))
  (define leo (new cat%))
  (eval:error (send leo meow 1/2))
  (eval:error (send leo meow -2))
  (send leo meow 4)]

@section{@racket[provide] Forms}

@defform[(exercise-out id ...)]{
  @racketlink[exercise-option]{Exercises} the given options before providing.

  @examples[#:eval evaluator
    (module inner racket
      (require contract-etc
               racket/contract/option)
      (provide (exercise-out foo)
               (rename-out [foo unchecked-foo]))
      (define/contract (foo)
        (option/c (-> integer?))
        "nan"))

    (require 'inner)
    (unchecked-foo)
    (eval:error (foo))]
}

@defform[(waive-out id ...)]{
  Similar to @racket[exercise-out], except it @racketlink[waive-option]{waives}
  the given options before providing.
}

@section{Annotations}

@defmodule[contract-etc/annotate]

Typically, programmers will only attach contracts at module or library
boundaries with @racket[contract-out] and not use contracts at the
definition level with @racket[define/contract]. This is because fine-grained
contract boundaries cause major performance problems due to the overhead
of repeated checking.

Contract annotations provide a convenient means of enabling and disabling
internal contract checks as needed. For example, you may decide that for
local testing you want to disable internal contract checks, but enable them
during continuous integration testing.

@defform[(: id contract-expr)]{
  Annotates the definition of @racket[id] with a contract.
  The first-order part of the contract is checked immediately.
  For a flat contract, nothing else needs to happen. For a
  higher-order contract, an @racketlink[option/c]{option} of
  @racket[contract-expr] is attached to @racket[id].

  Where, and whether, that option is enabled depends
  on the environment variables present at run time.

  @itemize[
    @item{If @indexed-envvar{CONTRACT_EXERCISE} is set,
      then the option is enabled by default.}

    @item{If @indexed-envvar{CONTRACT_EXERCISE_TEST} is
      set, then the option is enabled by default only
      in the test submodule of the current file.}

    @item{If neither are set, then the option is disabled
      by default.}]

  @examples[#:eval evaluator
    (: sub2 (-> number? number?))
    (eval:error (define (sub2) 42))

    (: add2 (-> integer? integer?))
    (define (add2 x)
      (+ x 2))

    (add2 1.5)
    (eval:error ((exercise-option add2) 1.5))]
}
