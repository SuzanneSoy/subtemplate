#lang typed/racket
(require (for-syntax syntax/parse
                     backport-template-pr1514/experimental/template
                     type-expander/expander)
         "../traversal.hl.rkt")

(provide define-fold)

(define-syntax define-fold
  (syntax-parser
    [(_ _function-name:id
        _type-name:id
        whole-type:type
        _type-to-replaceᵢ:type ...)
     (with-folds
      (λ ()
        (template
         (begin
           (define-type _type-name
             (∀-replace-in-type whole-type _type-to-replaceᵢ ...))
           (define _function-name
             (λ-replace-in-instance whole-type _type-to-replaceᵢ ...))))))]))