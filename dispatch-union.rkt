#lang typed/racket

(require phc-toolkit
         phc-adt
         (for-syntax racket/base
                     phc-toolkit/untyped
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template
                     type-expander/expander
                     "free-identifier-tree-equal.rkt")
         (for-meta 2 racket/base)
         (for-meta 2 phc-toolkit/untyped)
         (for-meta 2 syntax/parse))

(provide dispatch-union)

(define-syntax/parse (dispatch-union ([type-to-replaceᵢ Aᵢ predicateᵢ] …)
                                     [X v result] …)
  ((λ (x) (local-require racket/pretty) #;(pretty-write (syntax->datum x)) x)
   #`(cond
       . #,(stx-map
            (λ (X v result)
              (syntax-parse X
                #:literals (tagged)
                [(tagged name [fieldᵢ (~optional :colon) typeᵢ] …)
                 #`[((tagged? name fieldᵢ …) #,v) #,result]]
                [other (raise-syntax-error 'graph
                                           "Unhandled union type"
                                           #'other)]))
            #'(X …)
            #'(v …)
            #'(result …)))))