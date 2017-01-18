#lang hyper-literate typed/racket #:no-auto-require

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

       (define-syntax/parse (define-graph-type . :signature)
         (define gi <graph-info>)
         (local-require racket/pretty)
         #;(let ([old-print-convert-hook (current-print-convert-hook)])
           (parameterize ([constructor-style-printing #t]
                          [show-sharing #f]
                          [current-print-convert-hook
                           (syntax-convert old-print-convert-hook)])
             (parameterize ([pretty-print-columns 188])
               (pretty-write (print-convert gi)))))
         #`(begin
             (define-syntax name #,gi)))]

@chunk[<graph-info>
       (graph-info #'name
                   (syntax->list (if (attribute tvar) #'(tvar …) #'()))
                   #'root-node
                   (syntax->list #'(nodeᵢ …))
                   (make-immutable-hash
                    (map cons
                         (stx-map syntax-e #'(nodeᵢ …))
                         (stx-map (λ/syntax-case (nodeᵢ [fieldᵢⱼ τᵢⱼ] …) ()
                                    <node-info>)
                                  #'([nodeᵢ [fieldᵢⱼ τᵢⱼ] …] …))))
                   (list->equal-hash-set
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
                            phc-graph/subtemplate-override))

       (provide define-graph-type)
       
       <define-graph-type>]