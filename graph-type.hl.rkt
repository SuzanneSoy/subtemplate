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

@CHUNK[<define-graph-type>
       (begin-for-syntax
         (define-syntax-class signature
           #:datum-literals (∈ ∋ ≡ ≢ ∉)
           #:literals (:)
           (pattern (name
                     {~maybe #:∀ (tvar …)}
                     (~and {~seq [nodeᵢ:id [fieldᵢⱼ:id : τ] …] …}
                           {~seq [root-node . _] _ …})
                     {~seq #:invariant a {~and op {~or ∈ ∋ ≡ ≢ ∉}} b} …
                     {~seq #:invariant p} …))))

       ;; DEBUG
       (require (for-syntax mzlib/pconvert
                            racket/list))
       (define-for-syntax (to-datum v)
         (syntax->datum (datum->syntax #f v)))
       (define-for-syntax ((syntax-convert old-print-convert-hook)
                           val basic-convert sub-convert)
         (cond
           [(set? val)
            (cons 'set (map sub-convert (set->list val)))]
           [(and (hash? val) (immutable? val))
            (cons 'hash
                  (append-map (λ (p) (list (sub-convert (car p))
                                           (sub-convert (cdr p))))
                              (hash->list val)))]
           [(syntax? val)
            (list 'syntax (to-datum val))]
           [else
            (old-print-convert-hook val basic-convert sub-convert)]))

       (define-for-syntax compute-graph-info
         (syntax-parser
           [:signature <graph-info>]))
       (define-syntax/parse (define-graph-type . whole:signature)
         (local-require racket/pretty)
         ;; fire off the eventual errors within macro-expansion.
         (compute-graph-info #'whole)
         #`(begin
             (define-syntax whole.name
               (compute-graph-info (quote-syntax whole)))))]

@chunk[<graph-info>
       #:with (node-incompleteᵢ …) (stx-map #λ(format-id % " ~a-incomplete" %)
                                            #'(nodeᵢ …))
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
                  (check-remembered-node! #'(nodeᵢ fieldᵢⱼ …))
                  (meta-struct-constructor
                   (check-remembered-tagged! #'(node-incompleteᵢ fieldᵢⱼ …)))
                  (check-remembered-tagged! #'(node-incompleteᵢ fieldᵢⱼ …)))]

@chunk[<field-info>
       (field-info #'τᵢⱼ)]

@chunk[<invariant-info-op>
       (invariant-info #'predicateTODO
                       #'witnessTODO)]

@chunk[<invariant-info-p>
       (invariant-info #'predicateTODO
                       #'witnessTODO)]

@chunk[<*>
       (require racket/require
                phc-toolkit
                (lib "phc-adt/tagged-structure-low-level.hl.rkt")
                (for-syntax "graph-info.hl.rkt"
                            phc-toolkit/untyped
                            (subtract-in syntax/parse phc-graph/subtemplate)
                            racket/set
                            phc-graph/subtemplate-override
                            racket/syntax)
                (for-meta 2 racket/base))

       (provide define-graph-type)
       
       <define-graph-type>]