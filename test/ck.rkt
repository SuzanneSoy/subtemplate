#lang typed/racket/base

(require phc-toolkit
         (for-syntax racket/base
                     syntax/parse
                     type-expander/expander
                     phc-toolkit/untyped))

(provide check-equal?-values:)

(define-syntax check-equal?-values:
  (syntax-parser
    [(_ actual {~maybe :colon type} expected ...)
     (quasisyntax/top-loc this-syntax
       (check-equal?: (call-with-values (ann (λ () actual)
                                             (-> #,(if (attribute type)
                                                       #'type
                                                       #'AnyValues)))
                                        (λ l l))
                      (list expected ...)))]))
  