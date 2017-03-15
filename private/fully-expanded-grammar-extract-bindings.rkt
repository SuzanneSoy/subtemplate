#lang racket/base

;; This file is based on the file fully-expanded-grammar.rkt in the same folder.

(require syntax/parse
         phc-toolkit/untyped
         "optcontract.rkt"
         racket/list
         (for-template '#%kernel))

(provide extract-bindings)

(define acc (make-parameter #f))

(define/contract (acc! v)
  (-> identifier? void?)
  (set-box! (acc) (cons v (unbox (acc)))))

(define-syntax-class acc-id
  #:attributes ()
  (pattern {~and id:id
                 {~do (acc! #'id)}}))

(define/contract (extract-bindings e)
  (-> syntax? (listof identifier?))
  (parameterize ([acc (box '())])
    (syntax-parse e
      [:expr 'ok])
    (fold-syntax (λ (stx rec)
                   (let ([d (syntax-property stx 'disappeared-binding)])
                     (for-each acc! (filter identifier? (flatten d))))
                   (rec stx))
                 e)
    (unbox (acc))))

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
               #%app #%top #%expression #%variable-reference
               define-values)
  (pattern :id)
  (pattern (lambda :formals :expr …+))
  (pattern (case-lambda (:formals :expr …+) …))
  (pattern (if :expr :expr :expr))
  (pattern (begin :expr …+))
  (pattern (begin0 :expr :expr …))
  (pattern (let-values ([(:acc-id …) :expr] …)
             :expr …+))
  (pattern (letrec-values ([(:acc-id …) :expr] …)
             :expr …+))
  (pattern (letrec-syntaxes+values ([(:acc-id …) :expr] …)
             ([(:acc-id …) :expr] …)
             :expr …+))
  (pattern (set! :id :expr))
  (pattern (quote _datum))
  (pattern (quote-syntax _datum))
  (pattern (quote-syntax _datum #:local))
  (pattern (with-continuation-mark :expr :expr :expr))
  (pattern (#%app :expr …+))
  (pattern (#%top . :id))
  (pattern (#%expression :expr))
  (pattern (#%variable-reference :id))
  (pattern (#%variable-reference (#%top . :id)))
  (pattern (#%variable-reference))
  (pattern (define-values (lifted-id:acc-id …) _lifted-expr)))

(define-syntax-class formals
  (pattern (:acc-id …))
  (pattern (:acc-id …+ . :acc-id))
  (pattern :acc-id))
