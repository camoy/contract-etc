#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide (all-from-out racket/contract)
         (contract-out
          [dynamic->d (-> (unconstrained-domain-> contract?) contract?)]
          [self/c (->* ((-> any/c contract?))
                       (#:chaperone? boolean?)
                       contract?)])
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
         racket/contract/option
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `apply/c` and `return/c` functions

;; A `decl` represents an `apply/c` or `return/c` declaration.
;;   - `contract` receieves a value on application or return.
;;   - `value` is the value to send to the contract.
;;   - `positive?` indicates if the blame should be positive.
(struct decl (contract value positive?))

;; Constructor for `apply/c` and `return/c`.
(define (apply/return-contract apply? decls)
  (make-chaperone-contract
   #:name (if apply? 'apply/c 'return/c)
   #:first-order procedure?
   #:late-neg-projection (apply/return-late-neg apply? decls)))

;; Constructor for the late neg of `apply/c` and `return/c`. It wraps a
;; procedure's call and return events, pushing those through the associated
;; contract.
(define (((apply/return-late-neg apply? decls) blm) proc neg)
  (define blm* (blame-add-missing-party blm neg))
  (define (plain-proc . args)
    (when apply?
      (apply/return-attach decls blm*))
    (define (result-wrapper-proc . res)
      (unless apply?
        (apply/return-attach decls blm*))
      (apply values res))
    (apply values (cons result-wrapper-proc args)))
  (chaperone-procedure
   proc
   (make-keyword-procedure
    (λ (kws kw-args . args)
      (apply plain-proc (append args (map list kws kw-args))))
    plain-proc)))

;; Attaches the contracts to the given values.
(define (apply/return-attach decls blm)
  (for ([decl (in-list decls)])
    (match-define (list ctc val pos?) decl)
    (define blame-swap-maybe (if pos? values blame-swap))
    (define blm* (blame-swap-maybe blm))
    (define pos (blame-positive blm*))
    (define neg (blame-negative blm*))
    (contract ctc val pos neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `apply/c` and `return/c` macros

(begin-for-syntax
  ;; A `apply/c` or `return/c` declaration.
  (define-splicing-syntax-class decl
    #:description "apply/c or return/c declaration"
    #:attributes (norm)
    (pattern [ctc val:expr (~optional (~and #:positive positive))]
             #:declare ctc (expr/c #'contract? #:name "contract")
             #:with norm #'(list ctc.c
                                 val
                                 (~? 'positive #f)))))

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
;; tests

(module+ test
  (require chk
           racket/function)

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
          (contract (apply/c [integer? "hi" #:positive]) values 'pos 'neg))
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
   "blaming: neg"
   ))
