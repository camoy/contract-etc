#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [dynamic->d (-> (-> any/c ... contract?) contract?)]
  [self/c (->* ((-> any/c contract?))
               (#:chaperone? boolean?)
               contract?)])

 apply/c
 return/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
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

;; Constructor for `apply/c` and `return/c`.
(define (apply/return-contract apply? pos? decls)
  (make-chaperone-contract
   #:name (if apply? 'apply/c 'return/c)
   #:first-order procedure?
   #:late-neg-projection (apply/return-late-neg apply? pos? decls)))

;; Constructor for the late neg of `apply/c` and `return/c`. It wraps a
;; procedure's call and return events, pushing those through the associated
;; contract.
(define (((apply/return-late-neg apply? pos? decls) blm) proc neg)
  (define blame-swap-maybe (if pos? values blame-swap))
  (define blm* (blame-swap-maybe (blame-add-missing-party blm neg)))
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
  (define pos (blame-positive blm))
  (define neg (blame-negative blm))
  (for ([decl (in-list decls)])
    (match-define (list ctc val) decl)
    (contract ctc val pos neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `apply/c` and `return/c` macros

(begin-for-syntax
  ;; A `apply/c` or `return/c` declaration.
  (define-splicing-syntax-class decl
    #:description "apply/c or return/c declaration"
    #:attributes (norm)
    (pattern [ctc val:expr]
             #:declare ctc (expr/c #'contract? #:name "contract")
             #:with norm #'(list ctc.c val))))

;; Trigger contract attachment on application.
(define-syntax (apply/c stx)
  (syntax-parse stx
    [(_ (~optional (~and #:positive positive)) d:decl ...+)
     #'(apply/return-contract #t
                              (~? 'positive #f)
                              (list d.norm ...))]))

;; Trigger contract attachment on return.
(define-syntax (return/c stx)
  (syntax-parse stx
    [(_ (~optional (~and #:positive positive)) d:decl ...+)
     #'(apply/return-contract #f
                              (~? 'positive #f)
                              (list d.norm ...))]))
