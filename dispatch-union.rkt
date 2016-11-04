#lang typed/racket/base

(require racket/require
         phc-toolkit
         phc-adt
         (for-syntax racket/base
                     phc-toolkit/untyped
                     racket/syntax
                     racket/format
                     syntax/parse
                     syntax/parse/experimental/template
                     type-expander/expander
                     "free-identifier-tree-equal.rkt")
         (for-meta 2 racket/base)
         (for-meta 2 phc-toolkit/untyped)
         (for-meta 2 syntax/parse))

(provide dispatch-union)

(define-syntax/parse (dispatch-union v
                                     ([type-to-replaceᵢ Aᵢ predicateᵢ] …)
                                     [Xⱼ resultⱼ] …)
  (define-syntax-class to-replace
    (pattern [t result]
             #:with (_ predicate)
             (findf (λ (r) (free-id-tree=? #'t (stx-car r)))
                    (syntax->list
                     #'([type-to-replaceᵢ predicateᵢ] …)))
             #:with clause #`[(predicate v) result]))
  
  (define-syntax-class tagged
    #:literals (tagged)
    (pattern [(tagged name [fieldₖ (~optional :colon) typeₖ] …) result]
             #:with clause #`[((tagged? name fieldₖ …) v) result]))

  (define-syntax-class other
    (pattern [other result]
             #:with clause #`[else result]))

  
  ((λ (x) (local-require racket/pretty) #;(pretty-write (syntax->datum x)) x)
   (syntax-parse #'([Xⱼ resultⱼ] …)
     [({~or to-replace:to-replace
            tagged:tagged
            {~between other:other 0 1
                      #:too-many (~a "only one non-tagged type can be part of"
                                     " the union")}}
       …)
      (quasisyntax/top-loc stx
        (cond 
          to-replace.clause …
          tagged.clause …
          other.clause …))])))