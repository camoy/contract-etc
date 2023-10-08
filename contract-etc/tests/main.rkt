#lang racket/base

(require chk
	 contract-etc
	 racket/class
	 racket/function
	 racket/match
	 racket/set
	 rackunit)

(require/expose contract-etc (minimum-arity maximum-arity))

(define increasing/c
  (dynamic->d
    (λ (x)
      (-> integer? (>/c x)))))

(define cdr-returns-car/c
  (and/c
    (self/c
      (λ (p)
	(match-define (cons x f) p)
	(cons/c any/c (-> x))))
    (cons/c integer? (-> integer?))))

(chk
  (contract (elementof/c integer? car) (cons 1 2) 'pos 'neg)
  (cons 1 2)

  #:x (contract (elementof/c integer? car) (cons "" 2) 'pos 'neg)
  "promised: integer?"

  #:do (define/contract (f* . args)
	 (case->i (->i ([x integer?]) [_ void?])
		  (-> integer? string? void?))
	 (void))

  (f* 1) (void)
  (f* 1 "hi") (void)
  #:x (f* 1 2) "expected: string?"
  #:x (case->i (-> integer? integer?) integer?)
  "expected: arrow-contract?"

  #:do (define (arity-range x)
	 (values (minimum-arity x)
		 (maximum-arity x)))
  (arity-range (-> void?))
  (values 0 0)
  (arity-range (-> integer? void?))
  (values 1 1)
  (arity-range (-> integer? ... void?))
  (values 0 #f)
  (arity-range (->* () void?))
  (values 0 0)
  (arity-range (->* (integer?) void?))
  (values 1 1)
  (arity-range (->* (integer?) (integer?) void?))
  (values 1 2)
  (arity-range (->* (integer? integer?) (integer?) void?))
  (values 2 3)
  (arity-range (->* (integer? integer?) #:rest any/c void?))
  (values 2 #f)
  (arity-range (->i ([x integer?]) [_ void?]))
  (values 1 1)
  (arity-range (->i ([x integer?]
		     [y (x) integer?])
		    ([z (y) integer?])
		    [_ void?]))
  (values 2 3)
  (arity-range (->i ([x integer?]
		     [y (x) integer?])
		    #:rest [r any/c]
		    [_ void?]))
  (values 2 #f)

  #:do (define/contract add1* increasing/c add1)
  (add1* 42) 43
  #:do (define/contract values* increasing/c values)
  #:x (values* 42)
  "produced: 42"

  #:do (define/contract good-self cdr-returns-car/c (cons 1 (const 1)))
  ((cdr good-self)) 1
  #:do (define/contract bad-self cdr-returns-car/c (cons 1 (const 2)))
  #:x ((cdr bad-self))
  "produced: 2"

  #:do (define/contract good-apply (apply/c [integer? 1]) values)
  (good-apply 2) 2
  #:do (define bad-apply-neg
	 (contract (apply/c [integer? "hi"])
		   (λ (x) (error "yikes"))
		   'pos 'neg))
  #:x (bad-apply-neg 2)
  "blaming: neg"
  #:do (define bad-apply-pos
	 (contract (apply/c [integer? "hi" #:swap]) values 'pos 'neg))
  #:x (bad-apply-pos 2)
  "blaming: pos"

  #:do (define/contract good-return (return/c [integer? 1]) values)
  (good-return 2) 2
  #:do (define good-return-escape
	 (contract (return/c [integer? "hi"])
		   (λ (k) (k))
		   'pos 'neg))
  #:t (begin (let/cc k (good-return-escape k)) #t)
  #:do (define bad-return
	 (contract (return/c [integer? "hi"]) values 'pos 'neg))
  #:x (bad-return 2)
  "blaming: pos"

  #:do (define woody%
	 (class object%
	   (define/public (draw who)
	     (format "reach for the sky, ~a" who))
	   (super-new)))
  #:do (define next!
	 (let ([x -1])
	   (λ ()
	     (set! x (add1 x))
	     x)))
  #:do (define-values (put! get)
	 (let ([store null])
	   (values (λ (x) (set! store (cons x store)))
		   (λ () (reverse store)))))
  #:do (define ((symbol-put? x y) v)
	 (and (symbol? v)
	      (put! (list x y))))
  #:do (define/contract woody+c%
	 (class-object/c
	   (self/c
	     (λ (x)
	       (define n (next!))
	       (class/c [draw (->m (symbol-put? n 'class-draw) string?)])))
	   (self/c
	     (λ (x)
	       (define n (next!))
	       (object/c [draw (->m (symbol-put? n 'object-draw) string?)]))))
	 woody%)

  #:do (define w1 (new woody+c%))
  #:do (define w2 (new woody+c%))
  (send w1 draw 'alice)
  "reach for the sky, alice"
  (send w2 draw 'bob)
  "reach for the sky, bob"
  #:x (send w1 draw 42)
  "draw: contract violation"
  #:eq set=?
  (get)
  '((1 object-draw) (0 class-draw) (2 object-draw) (0 class-draw))

  #:do (define foo<%>
	 (interface ()
	   [foo (->m positive? any)]))
  #:do (define/contract foo%
	 (class-object/c
	   (class/c)
	   (object/c
	     [foo (->m even? any/c)]))
	 (class* object% (foo<%>)
	   (super-new)
	   (define/public (foo x) x)))

  #:do (define f (new foo%))
  #:x (send f foo -10)
  "expected: positive?"

  #:x (send f foo 3)
  "expected: even?"

  #:do (define/contract bar%
	 (dependent-class-object/c
	   (class/c)
	   (λ (#:hello hi #:goodbye bye . more)
	     (object/c
	       [bar (->m (λ (x) (equal? x hi))
			 (λ (y) (equal? y bye))
			 (λ (z) (equal? z more))
			 any)])))
	 (class* object% ()
	   (init hello goodbye)
	   (init-rest more)
	   (super-new)
	   (define/public (bar x y z)
	     #t)))

  #:do (define b (new bar% [goodbye 11] [hello 10]))
  #:do (define c (make-object bar% 10 11 12 13))
  #:t (send b bar 10 11 '())
  #:t (send c bar 10 11 (list 12 13))
  )
