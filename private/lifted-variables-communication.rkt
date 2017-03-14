#lang racket/base

(provide lift-late-pvars-param
         (for-syntax lift-late-pvars-target
                     lifted-pvar
                     x-lifted-pvar-marker))

(require racket/stxparam
         (for-syntax racket/base
                     racket/syntax
                     "optcontract.rkt";racket/contract
                     ))

(define-syntax-parameter lift-late-pvars-param #f)

(define-for-syntax (lift-late-pvars-target)
  (syntax-parameter-value #'lift-late-pvars-param))

(define-for-syntax x-lifted-pvar-marker (make-syntax-introducer))

;; Returns two values, the syntax to insert, and a symbol to use at run-time
;; to access the value of that lifted pvar.
(begin-for-syntax
  (define/contract (lifted-pvar name macro+args-stx)
    (-> symbol? syntax? (cons/c symbol? syntax?))
    (define lifted-symbol (gensym (format "lifted-~a" name)))
    (define lifted-hint-id (generate-temporary lifted-symbol))
    (cons lifted-symbol
          (syntax-property (x-lifted-pvar-marker lifted-hint-id)
                           'lifted-pvar
                           (cons lifted-symbol macro+args-stx)))))