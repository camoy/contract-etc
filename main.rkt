#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide (all-from-out racket/contract)
         (contract-out
          [dynamic->d (-> (unconstrained-domain-> contract?) contract?)]
          [self/c (->* ((-> any/c contract?))
                       (#:chaperone? boolean?)
                       contract?)]
          [class-object/c (-> contract? contract? contract?)])
         self-rec/c
         apply/c
         return/c
         exercise-out
         waive-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/provide-transform)
         (only-in racket/private/class-internal
                  struct:class
                  class-make-object
                  set-class-make-object!)
         racket/contract/option
         racket/class
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dynamic->d`

;; Contract that is attached to a procedure after it is called (provided its
;; arguments). This can be used as a sort of dynamic ->d.
(define (dynamic->d mk-ctc)
  (make-contract
   #:name 'dynamic->d
   #:first-order procedure?
   #:late-neg-projection
   (λ (blm)
     (λ (proc neg)
       (unless (procedure? proc)
         (raise-blame-error
          blm proc
          '(expected: "procedure?" given: "~e")
          proc))
       (λ args
         (define ctc (apply mk-ctc args))
         (define late-neg (get/build-late-neg-projection ctc))
         (apply ((late-neg blm) proc neg) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `self/c`

;; This allows one to attach a contract to a value that depends on the carrier
;; itself.
(define (self/c make-ctc #:chaperone? [chaperone? #f])
  ((if chaperone? make-chaperone-contract make-contract)
   #:name 'self/c
   #:late-neg-projection
   (λ (blm)
     (λ (arg neg-party)
       (define late-neg-proj (get/build-late-neg-projection (make-ctc arg)))
       ((late-neg-proj blm) arg neg-party)))))

;; This allows one to attach a contract to a value that depends on the carrier
;; itself, where the carrier is protected.
(define-syntax (self-rec/c stx)
  (syntax-parse stx
    [(_ name:id e:expr
        (~optional (~and #:chaperone (~bind [make #'make-chaperone-contract]))
                   #:defaults ([make #'make-contract])))
     #'(make
        #:late-neg-projection
        (λ (blm)
          (λ (arg neg-party)
            (letrec ([late-neg-proj (get/build-late-neg-projection e)]
                     [name ((late-neg-proj blm) arg neg-party)])
              name))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `apply/c` and `return/c` functions

;; A `decl` represents an `apply/c` or `return/c` declaration.
;;   - `contract` receieves a value on application or return,
;;   - `value` is the value to send to the contract,
;;   - `swap?` indicates if the blame should be swapped.
(struct decl (contract value swap?))

;; Constructor for `apply/c` and `return/c`.
(define (apply/return-contract apply? decls)
  (define name (if apply? 'apply/c 'return/c))
  (make-chaperone-contract
   #:name name
   #:first-order procedure?
   #:late-neg-projection (apply/return-late-neg name apply? decls)))

;; Constructor for the late neg of `apply/c` and `return/c`. It wraps a
;; procedure's call and return events, pushing those through the associated
;; contract.
(define (apply/return-late-neg name apply? decls)
  (define late-negs (apply/return-late-negs name decls))
  (define vals (map decl-value decls))
  (λ (blm)
    (define projs (apply/return-projs apply? decls late-negs blm))
    (λ (proc neg)
      (define (plain-proc . args)
        (when apply? (apply/return-attach projs vals neg))
        (define (result-wrapper-proc . res)
          (unless apply? (apply/return-attach projs vals neg))
          (apply values res))
        (apply values (cons result-wrapper-proc args)))
      (chaperone-procedure
       proc
       (make-keyword-procedure
        (λ (kws kw-args . args)
          (apply plain-proc (append args (map list kws kw-args))))
        plain-proc)))))

;; Constructs late neg projections for each declaration.
(define (apply/return-late-negs name decls)
  (for/list ([decl (in-list decls)])
    (define ctc (coerce-contract name (decl-contract decl)))
    (get/build-late-neg-projection ctc)))

;; Constructs projections for each declaration from late negs.
(define (apply/return-projs apply? decls late-negs blm)
  (for/list ([decl (in-list decls)]
             [late-neg (in-list late-negs)])
    (let* ([blm (if apply? (blame-swap blm) blm)]
           [blm (if (decl-swap? decl) (blame-swap blm) blm)])
      (late-neg blm))))

;; Attaches the contracts to the given values.
(define (apply/return-attach projs vals neg)
  (for ([proj (in-list projs)]
        [val (in-list vals)])
    (proj val neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `apply/c` and `return/c` macros

(begin-for-syntax
  ;; A `apply/c` or `return/c` declaration.
  (define-splicing-syntax-class decl
    #:description "apply/c or return/c declaration"
    #:attributes (norm)
    (pattern [ctc val:expr (~optional (~and #:swap swap?))]
             #:declare ctc (expr/c #'contract? #:name "contract")
             #:with norm #'(decl ctc.c val (~? 'swap? #f)))))

;; Trigger contract attachment on application.
(define-syntax (apply/c stx)
  (syntax-parse stx
    [(_  d:decl ...+)
     #'(apply/return-contract #t (list d.norm ...))]))

;; Trigger contract attachment on return.
(define-syntax (return/c stx)
  (syntax-parse stx
    [(_ d:decl ...+)
     #'(apply/return-contract #f (list d.norm ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide transformers

(begin-for-syntax
  ;; Applies `proc` to the provides before exporting.
  (define (make-option-provide-pre-transformer proc)
    (make-provide-pre-transformer
     (λ (stx modes)
       (syntax-parse stx
         [(_ ?x ...)
          #:with (?x-new ...) (generate-temporaries #'(?x ...))
          ;; Since we're directly applying `proc`, we can't use the standard
          ;; `syntax-local-lift-expression` as you'd get an unbound identifier on
          ;; the RHS. Instead, we define everything at the end of the module and
          ;; then provide there.
          (syntax-local-lift-module-end-declaration
           #`(begin
               (define ?x-new (#,proc ?x)) ...
               (provide (rename-out [?x-new ?x] ...))))
          ;; This is an empty export.
          #'(combine-out)])))))

;; Provide options after exercising.
(define-syntax exercise-out
  (make-option-provide-pre-transformer #'exercise-option))

;; Provide options after waiving.
(define-syntax waive-out
  (make-option-provide-pre-transformer #'waive-option))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `class-object/c`

;; Constructs a class contract with an additional object constraint.
(define (class-object/c class-ctc obj-ctc)
  (make-contract
   #:name
   `(class-object/c ,(contract-name class-ctc)
                    ,(contract-name obj-ctc))
   #:first-order class?
   #:late-neg-projection
   (class-object/c-late-neg
    (get/build-late-neg-projection class-ctc)
    (get/build-late-neg-projection obj-ctc))))

;; Late neg projection for `class-object/c`.
(define (class-object/c-late-neg class-proj obj-proj)
  (λ (blm)
    (define class-blm (blame-add-context blm "the class contract of"))
    (define class-proj+blm (class-proj class-blm))
    (define obj-blm (blame-add-context blm "the object contract of"))
    (define obj-proj+blm (obj-proj obj-blm))
    (λ (val neg)
      (impersonate-struct
       (class-proj+blm val neg)
       struct:class
       class-make-object
       (impersonate-make-object obj-proj+blm neg)
       set-class-make-object!
       set-class-make-object!))))

;; Impersonator for the `make-object` field of the internal class struct.
(define (impersonate-make-object obj-proj+blm neg)
  (λ (self mk-object)
    (impersonate-procedure
     mk-object
     (λ ()
       (λ (res)
         (obj-proj+blm res neg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/function
           racket/set)

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
   ))
