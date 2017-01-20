#lang aful/unhygienic hyper-literate typed/racket #:no-auto-require

@require[scribble-math
         scribble-enhanced/doc
         "notations.rkt"
         (for-label racket)]

@title[#:style (with-html5 manual-doc-style)
       #:tag "graph-type"
       #:tag-prefix "phc-graph/graph-type"]{Declaring graph types}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/graph-type"))

The @racket[define-graph-type] form binds @racket[name] to a
@racket[graph-info] struct. The @racket[name] therefore contains metadata
describing among other things the types of nodes, the invariants that
instances of this graph type will satisfy.

@chunk[<signature>
       (begin-for-syntax
         (define-syntax-class signature
           #:datum-literals (∈ ∋ ≡ ≢ ∉)
           #:literals (:)
           (pattern
            (~no-order {~once name}
                       {~maybe #:∀ (tvar …)}
                       {~once (~and {~seq (nodeᵢ:id [fieldᵢⱼ:id : τᵢⱼ:type]
                                                    …) …}
                                    {~seq [root-node . _] _ …})}
                       {~seq #:invariant a {~and op {~or ∈ ∋ ≡ ≢ ∉}} b}
                       {~seq #:invariant p}))))]

@section{Implementation}

The @racket[define-graph-type] macro expands to code which defines names for
the node types. It then binds the given @racket[name] to the
@racket[graph-info] instance built by @racket[build-graph-info].

@CHUNK[<define-graph-type>
       (begin-for-syntax
         (define-template-metafunction (!check-remembered-node! stx)
           (syntax-case stx ()
             [(_ nodeᵢ fieldᵢⱼ …)
              (syntax-local-template-metafunction-introduce
               (check-remembered-node! #'(nodeᵢ fieldᵢⱼ …)))])))
       
       (define-syntax/parse (define-graph-type . {~and whole :signature})
         ;; fire off the eventual delayed errors added by build-graph-info
         (lift-maybe-delayed-errors)
         #`(begin
             <declare-node-types>
             (define-syntax name
               (build-graph-info (quote-syntax whole)))))]

@section{Declaring the node types}

@chunk[<declare-node-types>
       (define-type nodeᵢ
         (Promise
          ((!check-remembered-node! nodeᵢ fieldᵢⱼ …) τᵢⱼ …
                                                     'Database
                                                     'Index)))
       …]

@section{Creating the @racket[graph-info] instance}

@CHUNK[<build-graph-info>
       (define-for-syntax (build-graph-info stx)
         (parameterize ([disable-remember-immediate-error #t])
           (syntax-parse stx
             [:signature
              <graph-info>])))]

@chunk[<graph-info>
       (graph-info #'name
                   (syntax->list (if (attribute tvar) #'(tvar …) #'()))
                   #'root-node
                   (syntax->list #'(nodeᵢ …))
                   (make-immutable-hash
                    (map cons
                         (stx-map syntax-e #'(nodeᵢ …))
                         (stx-map (λ/syntax-case (nodeᵢ node-incompleteᵢ
                                                        [fieldᵢⱼ τᵢⱼ] …) ()
                                    <node-info>)
                                  #'([nodeᵢ node-incompleteᵢ
                                      [fieldᵢⱼ τᵢⱼ] …] …))))
                   (list->set
                    (append
                     (stx-map (λ/syntax-case (op a b) () <invariant-info-op>)
                              #'([op a b] …))
                     (stx-map (λ/syntax-case p () <invariant-info-p>)
                              #'(p …)))))]

@chunk[<node-info>
       (node-info (meta-struct-predicate
                   (check-remembered-node! #'(nodeᵢ fieldᵢⱼ …)))
                  (syntax->list #'(fieldᵢⱼ …))
                  (make-immutable-hash
                   (map cons
                        (stx-map syntax-e #'(fieldᵢⱼ …))
                        (stx-map (λ/syntax-case (fieldᵢⱼ τᵢⱼ) ()
                                   <field-info>)
                                 #'([fieldᵢⱼ τᵢⱼ] …))))
                  #'nodeᵢ ; promise type
                  #;(meta-struct-constructor
                   (check-remembered-tagged! #'(node-incompleteᵢ fieldᵢⱼ …)))
                  #;(check-remembered-tagged! #'(node-incompleteᵢ fieldᵢⱼ …)))]

@chunk[<field-info>
       (field-info #'τᵢⱼ)]

@chunk[<invariant-info-op>
       (invariant-info #'predicateTODO
                       #'witnessTODO)]

@chunk[<invariant-info-p>
       (invariant-info #'predicateTODO
                       #'witnessTODO)]

@section{Putting it all together}

@chunk[<*>
       (require racket/require
                phc-toolkit
                remember
                (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                (for-syntax "graph-info.hl.rkt"
                            type-expander/expander
                            phc-toolkit/untyped
                            (subtract-in syntax/parse phc-graph/subtemplate)
                            racket/set
                            phc-graph/subtemplate-override
                            racket/syntax
                            extensible-parser-specifications
                            backport-template-pr1514/experimental/template)
                (for-meta 2 racket/base))

       (provide define-graph-type)

       <signature>
       <build-graph-info>
       <define-graph-type>]