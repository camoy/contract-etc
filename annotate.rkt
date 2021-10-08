#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out [annotate :]
             [define-annotated define]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/id-table
                     syntax/parse/lib/function-header

                     debug-scopes)
         racket/contract
         racket/contract/option)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

;; String
;; Environment variable that determines if the test submodule will have
;; annotated contracts enabled.
(define EXERCISE-TEST "CONTRACT_EXERCISE_TEST")

;; String
;; Environment variable that determines if annotated contracts are enabled.
(define EXERCISE "CONTRACT_EXERCISE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(begin-for-syntax
  ;; [Bound-Id-Table Identifier Syntax]
  ;; Table that stores contract annotations.
  (define contract-table (make-bound-id-table))

  ;; Syntax class for the head of a `define` form that exports its name.
  (define-syntax-class define-header
    (pattern x:function-header #:with name #'x.name)
    (pattern x:id #:with name #'x))

  ;; Returns whether the given syntax came from within a `test` submodule.
  (define (syntax-inside-test? stx)
    (define src-mod (syntax-source-module stx))
    (define base-path
      (and src-mod
           (let-values ([(_ base-path) (module-path-index-split src-mod)])
             base-path)))
    (define submods (and base-path (module-path-index-submodule base-path)))
    (and submods (member 'test submods)))
  )

;; Annotates the given identifier with a contract.
(define-syntax (annotate stx)
  (syntax-parse stx
    [(_ ?name:id ?ctc)
     #:declare ?ctc (expr/c #'contract?)
     #:do [(define name-stx (syntax-local-introduce #'?name))
           (bound-id-table-set! contract-table name-stx #'?ctc.c)]
     #'(void)]))

;; Creates a definition with the contract attached as an option where the
;; first-order test is always checked. Whether the contract is exercised
;; depends on the `EXECISE` and `EXERCISE_TEST` environment variables.
(define-syntax (define-annotated stx)
  (syntax-parse stx
    [(_ ?head:define-header ?body:expr ...+)
     #:do [(define name-stx (syntax-local-introduce #'?head.name))]
     #:attr ?ctc (bound-id-table-ref contract-table name-stx (const #f))
     #:do [(define in-test? (syntax-inside-test? #'?head.name))]
     #:with ?in-test (if in-test? #'#t #'#f)
     #:attr ?test-defn
     (and (not in-test?)
          (eq? (syntax-local-context) 'module)
          #'(define-in-test ?head.name))
     #`(begin
         (define/contract ?head
           (option/c-from-env (~? ?ctc any/c) ?in-test)
           ?body ...)
         (~? ?test-defn))]))

;; Possibly redefines the given identifier to exercise the contract in the
;; test submodule.
(define-syntax (define-in-test stx)
  (syntax-parse stx
    [(_ ?name:id)
     #:with ?alias (generate-temporary 'alias)
     #'(begin
         (define ?alias ?name)
         (module+ test
           (define ?name (exercise-from-env ?alias))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime

;; Wraps the given contract in an option. Whether it's enabled by default
;; depends. If `EXERCISE` is set, or `EXERCISE_TEST` is set and the we're
;; currently in the test submodule, then the contract is enabled.
(define (option/c-from-env ctc in-test?)
  (define exercise? (getenv EXERCISE))
  (define exercise-test? (getenv EXERCISE-TEST))
  (define first-order (contract-first-order ctc))
  (if (or (flat-contract? ctc) exercise?)
      ctc
      (option/c ctc
                #:with-contract (or exercise? (and in-test? exercise-test?))
                #:tester (contract-first-order ctc))))

;; If `EXERCISE_TEST` is set, exercise the given option.
(define (exercise-from-env val)
  (if (getenv EXERCISE-TEST)
      (exercise-option val)
      val))
