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
          [elementof/c (-> contract? (-> any/c any/c) contract?)]
          [case->i (-> arrow-contract? ... contract?)]
          [class-object/c (-> contract? contract? contract?)]
          [dependent-class-object/c (-> contract? procedure? contract?)]
          [classof/c (-> contract? contract?)]
          [dependent-classof/c (-> procedure? contract?)])
         apply/c
         return/c
         exercise-out
         waive-out)

(define arrow-contract?
  (flat-named-contract
   'arrow-contract?
   (λ (x)
     (or (arr:base->? x) (->i? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/provide-transform)
         (prefix-in int: racket/private/class-internal)
         (prefix-in arr: racket/contract/private/arrow-common)
         racket/contract/option
         racket/class
         racket/dict
         racket/match
         racket/unsafe/ops
         rackunit)

;; Needed for `class-object/c`.
(require/expose racket/private/class-internal (fetch-concrete-class))

;; Needed for `case->i`.
(require/expose racket/contract/private/arr-i
                (->i-mandatory-args ->i-opt-args ->i-rest ->i?))

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
         (define ctc (coerce-contract 'dynamic->d (apply mk-ctc args)))
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
       (define ctc (coerce-contract 'self/c (make-ctc arg)))
       (define late-neg-proj (get/build-late-neg-projection ctc))
       ((late-neg-proj blm) arg neg-party)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `elementof/c`

;; Contract that applies to some piece of a data structure, projected by `get`.
(define (elementof/c ctc get)
  (define ctc* (coerce-contract/f ctc))
  (define make
    (if (chaperone-contract? ctc*)
        make-chaperone-contract
        make-contract))
  (define lnp (get/build-late-neg-projection ctc*))
  (make
   #:name `(elementof/c ,(contract-name ctc*))
   #:late-neg-projection
   (λ (blm)
     (define lnp+blm (lnp blm))
     (λ (val neg)
       (lnp+blm (get val) neg)
       val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `case->i`

;; Like `case->`, but supports dependent contracts.
(define (case->i . ctcs)
  (define mins (map minimum-arity ctcs))
  (define maxs (map maximum-arity ctcs))
  (define chap? (andmap chaperone-contract? ctcs))
  (define make (if chap? make-chaperone-contract make-contract))
  (define chaperone/impersonate
    (if chap?
        unsafe-chaperone-procedure
        unsafe-impersonate-procedure))
  (define lnps (map get/build-late-neg-projection ctcs))
  (make #:name `(case->i ,(map contract-name ctcs))
        #:late-neg-projection
        (λ (blm)
          (define lnps+blm (map (λ (lnp) (lnp blm)) lnps))
          (λ (val neg)
            (define fs (map (λ (lnp) (lnp val neg)) lnps+blm))
            (chaperone/impersonate
             val
             (λ args
               (define n (length args))
               (for/first ([min-arity (in-list mins)]
                           [max-arity (in-list maxs)]
                           [f (in-list fs)]
                           #:when (and (<= min-arity n max-arity)))
                 (apply f args))))))))

(define (minimum-arity ctc)
  (cond
    [(->i? ctc) (->i-mandatory-args ctc)]
    [(arr:base->? ctc) (arr:base->-min-arity ctc)]))

(define (maximum-arity ctc)
  (cond
    [(->i? ctc)
     (and (not (->i-rest ctc))
          (+ (->i-mandatory-args ctc)
             (->i-opt-args ctc)))]
    [(arr:base->? ctc)
     (and (not (arr:base->-rest ctc))
          (length (arr:base->-doms ctc)))]))

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
  (define class-ctc* (coerce-contract 'class-object/c class-ctc))
  (define obj-ctc* (coerce-contract 'class-object/c obj-ctc))
  (make-contract
   #:name
   `(class-object/c ,(contract-name class-ctc*) ,(contract-name obj-ctc*))
   #:first-order class?
   #:late-neg-projection
   (class-object/c-late-neg
    (get/build-late-neg-projection class-ctc*)
    (get/build-late-neg-projection obj-ctc*))))

(define (class-object/c-late-neg class-proj obj-proj)
  (λ (blm)
    (define class-blm (blame-add-context blm "the class contract of"))
    (define class-proj+blm (class-proj class-blm))
    (define obj-blm (blame-add-context blm "the object contract of"))
    (define obj-proj+blm (obj-proj obj-blm))
    (λ (val neg)
      (define cls (class-proj+blm val neg))
      (define make-object*
        ;; Wrap call to `fetch-concrete-class` in object contract check.
        (let ([make (int:class-make-object (fetch-concrete-class cls blm))])
          (λ ()
           (obj-proj+blm (make) neg))))
      (copy-class-struct cls make-object* (int:class-init cls)))))

;; I would use `struct-copy` but the struct identifier isn't exported.
(define (copy-class-struct cls make-object* init*)
  (define name (int:class-name cls))
  ((int:make-naming-constructor int:struct:class name "class")
   name
   (int:class-pos cls)
   (int:class-supers cls)
   (int:class-self-interface cls)
   (int:class-insp-mk cls)
   (int:class-obj-inspector cls)
   (int:class-method-width cls)
   (int:class-method-ht cls)
   (int:class-method-ids cls)
   (int:class-abstract-ids cls)
   ;; Pretend that there's no interface contracts so that
   ;; `fetch-concrete-class` won't clobber `make-object`.
   null
   (int:class-ictc-classes cls)
   (int:class-methods cls)
   (int:class-super-methods cls)
   (int:class-int-methods cls)
   (int:class-beta-methods cls)
   (int:class-meth-flags cls)
   (int:class-inner-projs cls)
   (int:class-dynamic-idxs cls)
   (int:class-dynamic-projs cls)
   (int:class-field-width cls)
   (int:class-field-pub-width cls)
   (int:class-field-ht cls)
   (int:class-field-ids cls)
   (int:class-all-field-ids cls)
   (int:class-struct:object cls)
   (int:class-object? cls)
   make-object*
   (int:class-field-ref cls)
   (int:class-field-set! cls)
   (int:class-init-args cls)
   (int:class-init-mode cls)
   init*
   (int:class-orig-cls cls)
   (int:class-serializer cls)
   (int:class-fixup cls)
   (int:class-check-undef? cls)
   (int:class-no-super-init? cls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `dependent-class-object/c`

;; Constructs a class contract with an additional object constraint that depends
;; on the initialization arguments.
(define (dependent-class-object/c class-ctc make-obj-ctc)
  (define class-ctc* (coerce-contract 'dependent-class-object/c class-ctc))
  (make-contract
   #:name
   `(dependent-class-object/c
     ,(contract-name class-ctc*)
     ,(object-name make-obj-ctc))
   #:first-order class?
   #:late-neg-projection
   (dependent-class-object/c-late-neg
    (get/build-late-neg-projection class-ctc*)
    make-obj-ctc)))

;; The difference betweenn this an `class-object/c` is that we grab the
;; initialization arguments (which come after object construction) and
;; travel back in time with the use of `call/cc` to communication that
;; information to the object constructor. This is the only way, I'm
;; pretty sure, to do this without modifying `class-internal.rkt`.
(define (dependent-class-object/c-late-neg class-proj make-obj-ctc)
  (λ (blm)
    (define class-blm (blame-add-context blm "the class contract of"))
    (define class-proj+blm (class-proj class-blm))
    (define obj-blm (blame-add-context blm "the object contract of"))
    (λ (val neg)
      (define kont #f)
      (define cls (class-proj+blm val neg))
      (define make-object*
        (let ([make (int:class-make-object (fetch-concrete-class cls blm))])
          (λ ()
            (define inits (call/cc (λ (k) (set! kont k) #f)))
            (cond
              [inits
               (define-values (kw-dict rest-args) (inits->args inits))
               (define obj-ctc
                 (coerce-contract
                  'dependent-class-object/c
                  (keyword-apply/dict make-obj-ctc kw-dict rest-args)))
               (define obj-proj (get/build-late-neg-projection obj-ctc))
               (define obj-proj+blm (obj-proj obj-blm))
               (set! kont #f)
               (obj-proj+blm (make) neg)]
              [else (make)]))))
      (define init*
        (let ([init! (int:class-init cls)])
          (λ (o make-super c inited? leftovers named-args)
            (when kont (kont named-args))
            (init! o make-super c inited? leftovers named-args))))
      (copy-class-struct cls make-object* init*))))

;; Convert the dictionary of initialization arguments into two return values:
;; a keyword argument dictionary and a list of rest arguments.
(define (inits->args inits)
  (for/fold ([kw-dict null]
             [rest-args null]
             #:result (values kw-dict (reverse rest-args)))
            ([(k v) (in-dict inits)])
    (cond
      [k (define k* (symbol->keyword k))
         (values (cons (cons k* v) kw-dict) rest-args)]
      [else (values kw-dict (cons v rest-args))])))

(define (symbol->keyword sym)
  (string->keyword (symbol->string sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `classof/c` and `dependent-classof/c`

(define (classof/c ctc)
  (class-object/c (class/c) ctc))

(define (dependent-classof/c proc)
  (dependent-class-object/c (class/c) proc))