#lang racket/base

;; This file is not used by the project, but can be used as a base for macros
;; which need to parse the result of local-expand. For example, the file
;; fully-expanded-grammar-extract-bindings.rkt is based on this one.

(require syntax/parse
         phc-toolkit/untyped
         (for-template '#%kernel))

(provide top-level-form
         module-level-form
         submodule-form
         general-top-level-form
         expr
         formals)

(define-syntax-class top-level-form
  #:literals (#%expression module #%plain-module-begin begin begin-for-syntax)
  (pattern :general-top-level-form)
  (pattern (#%expression :expr))
  (pattern (module :id _module-path
             (#%plain-module-begin
              :module-level-form …)))
  (pattern (begin :top-level-form …))
  (pattern (begin-for-syntax :top-level-form …)))
 	 	 	 	 
(define-syntax-class module-level-form
  #:literals (#%provide begin-for-syntax #%declare)
  (pattern :general-top-level-form)
  (pattern (#%provide _raw-provide-spec …))
  (pattern (begin-for-syntax :module-level-form …))
  (pattern :submodule-form)
  (pattern (#%declare _declaration-keyword …)))
 	 	 	 	 
(define-syntax-class submodule-form
  #:literals (module #%plain-module-begin module* )
  (pattern (module :id _module-path
             (#%plain-module-begin
              :module-level-form …)))
  (pattern (module* :id _module-path
             (#%plain-module-begin
              :module-level-form …)))
  (pattern (module* :id #f
             (#%plain-module-begin
              :module-level-form …))))
  
(define-syntax-class general-top-level-form
  #:literals (define-values define-syntaxes #%require)
  (pattern :expr)
  (pattern (define-values (:id …) :expr))
  (pattern (define-syntaxes (:id …) :expr))
  (pattern (#%require _raw-require-spec …)))
 	 	 	 	 
(define-syntax-class expr
  #:literals (lambda case-lambda if begin begin0
               let-values letrec-values letrec-syntaxes+values
               set! quote quote-syntax
               with-continuation-mark
               #%app #%top #%expression #%variable-reference)
  (pattern :id)
  (pattern (#%plain-lambda :formals :expr …+))
  (pattern (case-lambda (:formals :expr …+) …))
  (pattern (if :expr :expr :expr))
  (pattern (begin :expr …+))
  (pattern (begin0 :expr :expr …))
             
  (pattern (let-values ([(:id …) :expr] …)
             :expr …+))
  (pattern (letrec-values ([(:id …) :expr] …)
             :expr …+))
  (pattern (letrec-syntaxes+values ([(:id …) :expr] …)
             ([(:id …) :expr] …)
             :expr …+))
  (pattern (set! :id :expr))
  (pattern (quote _datum))
  (pattern (quote-syntax _datum))
  (pattern (quote-syntax _datum #:local))
  (pattern (with-continuation-mark :expr :expr :expr))
  (pattern (#%plain-app :expr …+))
  (pattern (#%top . :id))
  (pattern (#%expression :expr))
  (pattern (#%variable-reference :id))
  (pattern (#%variable-reference (#%top . :id)))
  (pattern (#%variable-reference)))

(define-syntax-class formals
  (pattern (:id …))
  (pattern (:id …+ . :id))
  (pattern :id))