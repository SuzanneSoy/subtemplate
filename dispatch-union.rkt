#lang typed/racket/base

(require racket/require
         phc-toolkit
         phc-adt
         (for-syntax (subtract-in racket/base "subtemplate.rkt")
                     phc-toolkit/untyped
                     racket/syntax
                     (subtract-in syntax/parse "subtemplate.rkt")
                     syntax/parse/experimental/template
                     type-expander/expander
                     "free-identifier-tree-equal.rkt"
                     "subtemplate.rkt")
         (for-meta 2 racket/base)
         (for-meta 2 phc-toolkit/untyped)
         (for-meta 2 syntax/parse))

(provide dispatch-union)

(define-syntax/parse (dispatch-union v
                                     ([type-to-replaceᵢ Aᵢ predicateᵢ] …)
                                     [Xⱼ result] …)
  ((λ (x) (local-require racket/pretty) #;(pretty-write (syntax->datum x)) x)
   (quasisyntax/top-loc stx
     (cond
       ;; TODO: put first the type-to-replaceᵢ, then afterwards the other Xⱼ, otherwise it can fail to typecheck.
       . #,(stx-map
            (λ (Xⱼ result)
              (syntax-parse Xⱼ
                #:literals (tagged)
                [t
                 #:with (_ predicate)
                 (findf (λ (r) (free-identifier-tree=? #'t (stx-car r)))
                        (syntax->list
                         (subtemplate ([type-to-replaceᵢ predicateᵢ] …))))
                 #`[(predicate v) #,result]]
                [(tagged name [fieldₖ (~optional :colon) typeₖ] …)
                 #`[((tagged? name fieldₖ …) v) #,result]]
                [other (raise-syntax-error 'graph
                                           "Unhandled union type"
                                           #'other)]))
            #'(Xⱼ …)
            #'(result …))))))