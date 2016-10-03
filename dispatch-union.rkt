#lang typed/racket

(require phc-toolkit
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

(define-syntax/parse (dispatch-union ([type-to-replaceᵢ Aᵢ predicateᵢ] …)
                                     [X v result] …)
  (stx-map
   (λ (X v result)
     (cond
       [(meta-struct? X) #`[((struct-predicate #,X) #,v) #,result]]
       [else (raise-syntax-error 'graph "Unhandled union type" #'X)]))
   #'(X …)
   #'(v …)
   #'(result …)))