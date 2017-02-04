#lang racket/base

(provide lift-late-pvars-param
         (for-syntax lift-late-pvars-target))

(require racket/stxparam
         (for-syntax racket/base
                     racket/syntax
                     racket/contract))

(define-syntax-parameter lift-late-pvars-param #f)

(define-for-syntax (lift-late-pvars-target)
  (syntax-parameter-value #'must-lift-late-pvars?-param))

;; Returns two values, the syntax to insert, and a symbol to use at run-time
;; to access the value of that lifted pvar.
(begin-for-syntax
  (define/contract (lifted-pvar name expr-stx)
    (-> symbol? syntax? (values symbol? syntax?))
    (define lifted-symbol (gensym (format "lifted-~a" name)))
    (define lifted-hint-id (generate-temporary lifted-symbol))
    (values (syntax-property lifted-hint-id
                             'late-pvar
                             (cons lifted-symbol expr-stx))
            lifted-symbol)))