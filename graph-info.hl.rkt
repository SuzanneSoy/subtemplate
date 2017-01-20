#lang hyper-literate racket #:no-auto-require

@require[scribble-math
         scribble-enhanced/doc
         "notations.rkt"
         (for-label racket)]

@title[#:style (with-html5 manual-doc-style)
       #:tag "graph-info"
       #:tag-prefix "phc-graph/graph-info"]{Compile-time graph metadata}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/graph-info"))

We define here the compile-time metadata describing a graph type.

@section{Graph type information}

The type of a graph is actually the type of its constituent nodes. The node
types may be polymorphic in the @racket[_tvars] type variables. The root node
name and the order of the nodes are purely indicative here, as a reference to
any node in the graph instance would be indistinguishable from a graph rooted
in that node type.

The @racket[_invariants] are not enforced by the node types. Instead, the node
types just include the invariant type as a witness (inside the @racket[raw]
field). The invariant is enforced either by construction, or with a run-time
check performed during the graph creation.

@chunk[<graph-info>
       (struct+/contract graph-info
         ([name identifier?]
          [tvars (listof identifier?)]
          [root-node identifier?]
          [node-order (listof identifier?)]
          [nodes (hash/c symbol? node-info? #:immutable #t)]
          [invariants (set/c invariant-info? #:kind 'immutable #:cmp 'equal)])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'graph-info))]
         #:property prop:custom-print-quotable 'never)]

@;{
 Since sets created with @racket[set] cannot be used within syntax objects
 (they cannot be marshalled into compiled code), we fake sets using hashes with
 empty values:

 @chunk[<hash-set/c>
        (provide hash-set/c)
        (define/contract (hash-set/c elem/c
                                     #:kind [kind 'dont-care]
                                     #:cmp [cmp 'dont-care])
          (->* (chaperone-contract?)
               (#:kind (or/c 'dont-care 'immutable 'mutable
                             'weak 'mutable-or-weak)
                #:cmp (or/c 'dont-care 'equal 'eqv 'eq))
               contract?)
          (define immutable
            (case kind
              [(immutable)       #t]
              [(dont-care)       'dont-care]
              [else              #f]))
          (define h              (hash/c elem/c
                                         null?
                                         #:immutable immutable))
          (define cmp-contracts
            (case cmp
              [(dont-care)       empty]
              [(equal)           (list hash-equal?)]
              [(eqv)             (list hash-eqv?)]
              [(eq)              (list hash-eq?)]))
          (define weak-contracts
            (case kind
              [(weak)            (list hash-weak?)]
              ;; This is redundant: the mutable check is already included above
              [(mutable-or-weak) (list (or/c hash-weak? (not/c immutable?)))]
              [(dont-care)       empty]
              [else              (list (not/c hash-weak?))]))
          (if (empty? (append cmp-contracts weak-contracts))
              h
              (apply and/c (append (list h) cmp-contracts weak-contracts))))]

 @chunk[<hash-set/c>
        (provide equal-hash-set/c)
        (define/contract (equal-hash-set/c elem/c
                                           #:kind [kind 'dont-care])
          (->* (chaperone-contract?)
               (#:kind (or/c 'dont-care 'immutable 'mutable
                             'weak 'mutable-or-weak))
               contract?)
          (hash-set/c elem/c #:kind kind #:cmp 'equal))]

 @chunk[<hash-set/c>
        (provide list->equal-hash-set)
        (define/contract (list->equal-hash-set l)
          (-> (listof any/c) (equal-hash-set/c any/c #:kind 'immutable))
          (make-immutable-hash (map (λ (v) (cons v null)) l)))]
}

@section{Graph builder information}

The information about a graph type is valid regardless of how the graph
instances are constructed, and is therefore rather succinct.

The @racket[graph-builder-info] @racket[struct] extends this with meaningful
information about graph transformations. Two transformations which have the
same output graph type may use different sets of mapping functions.
Furthermore, the @racket[_dependent-invariants] are invariants relating the
input and output of a graph transformation.

The @racket[_multi-constructor] identifier refers to a function which takes
@${n} lists of lists of mapping argument tuples, and returns @${n} lists of
lists of nodes. It is the most general function allowing the creation of
instances of the graph. Wrappers which accept a single tuple of arguments and
return the corresponding node can be written based on it.

@chunk[<graph-builder-info>
       (struct+/contract graph-builder-info graph-info
         ([name identifier?]
          [tvars (listof identifier?)]
          [root-node identifier?]
          [node-order (listof identifier?)]
          [nodes (hash/c symbol? node-info? #:immutable #t)]
          [invariants (set/c invariant-info? #:kind 'immutable #:cmp 'equal)])
         ([multi-constructor identifier?]
          [root-mapping identifier?]
          [mapping-order (listof identifier?)]
          [mappings (hash/c symbol? mapping-info? #:immutable #t)]
          [dependent-invariants (set/c dependent-invariant-info?
                                       #:kind 'immutable
                                       #:cmp 'equal)])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'graph-builder-info))]
         #:property prop:custom-print-quotable 'never)]

@section{Node information}

@chunk[<node-info>
       (struct+/contract node-info
         ([predicate? identifier?]
          [field-order (listof identifier?)]
          [fields (hash/c symbol? field-info? #:immutable #t)]
          [promise-type stx-type/c]
          ;; Wrappers can mean that we have incomplete types with fewer
          ;; fields than the final node type.
          ;[make-incomplete-type identifier?]
          ;[incomplete-type identifier?]
          )
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'node-info))]
         #:property prop:custom-print-quotable 'never)]

@section{Field information}

A field has a type.

@chunk[<field-info>
       (struct+/contract field-info
         ([type stx-type/c])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'field-info))]
         #:property prop:custom-print-quotable 'never)]

@;[incomplete-type identifier?]

@section{Invariant information}

@chunk[<invariant-info>
       (struct+/contract invariant-info
         ([predicate identifier?] ; (→ RootNode Boolean : +witness-type)
          [witness-type stx-type/c])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'invariant-info))]
         #:property prop:custom-print-quotable 'never)]

@section{Dependent invariant information}

The invariants described in the previous section assert properties of a graph
instance in isolation. It is however desirable to also describe invariants
which relate the old and the new graph in a graph transformation.

@chunk[<dependent-invariant-info>
       (struct+/contract dependent-invariant-info
         ([checker identifier?] ; (→ RootMappingArguments… NewGraphRoot Boolean)
          [name identifier?])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'dependent-invariant-info))]
         #:property prop:custom-print-quotable 'never)]

@section{Mapping information}

@chunk[<mapping-info>
       (struct+/contract mapping-info
         ([mapping-function identifier?]
          [with-promises-type identifier?]
          [make-placeholder-type identifier?]
          [placeholder-type identifier?])
         #:transparent
         #:methods gen:custom-write
         [(define write-proc (struct-printer 'mapping-info))]
         #:property prop:custom-print-quotable 'never)]

@section{Printing}

It is much easier to debug graph information if it is free from the visual
clutter of printed syntax objects (which waste most of the screen real estate
printing @tt{#<syntax:/path/to/file}, when the interesting part is the
contents of the syntax object).

We therefore pre-process the fields, transforming syntax objects into regular
data.

@chunk[<printer>
       (define (to-datum v)
         (syntax->datum (datum->syntax #f v)))

       (define ((syntax-convert old-print-convert-hook)
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

       (define ((struct-printer ctor) st port mode)
         (match-define (vector name fields ...) (struct->vector st))
         (define-values (info skipped?) (struct-info st))
         (define-values (-short-name _2 _3 _4 _5 _6 _7 _8)
           (struct-type-info info))
         (define short-name (or ctor -short-name))
         (define (to-datum v)
           (syntax->datum (datum->syntax #f v)))
         (case mode
           [(#t)
            (display "#(~#t~" port)
            (display name port)
            (for-each (λ (f)
                        (display " " port)
                        (write (to-datum f) port))
                      fields)
            (display ")" port)]
           [(#f)
            (display "#(~#f~" port)
            (display name port)
            (for-each (λ (f)
                        (display " " port)
                        (display (to-datum f) port))
                      fields)
            (display ")" port)]
           [else
            (let ([old-print-convert-hook (current-print-convert-hook)])
              (parameterize ([constructor-style-printing #t]
                             [show-sharing #f]
                             [current-print-convert-hook
                              (syntax-convert old-print-convert-hook)])
                (write
                 (cons short-name
                       (map print-convert
                            ;; to-datum doesn't work if I map it on the fields?
                            fields))
                 port)))]))]

@CHUNK[<*>
       (require phc-toolkit/untyped
                type-expander/expander
                racket/struct
                mzlib/pconvert
                (for-syntax phc-toolkit/untyped
                            syntax/parse
                            syntax/parse/experimental/template
                            racket/syntax))
       
       (define-syntax/parse
           (struct+/contract name {~optional parent}
             {~optional ([parent-field parent-contract] ...)}
             ([field contract] ...)
             {~optional {~and transparent #:transparent}}
             (~and {~seq methods+props ...}
                   (~seq (~maybe #:methods
                                 {~literal gen:custom-write}
                                 _)
                         (~maybe #:property
                                 {~literal prop:custom-print-quotable}
                                 _)))
             {~optional {~and prefab #:prefab}})
         #:with name/c (format-id #'name "~a/c" #'name)
         ;(quasisyntax/loc (stx-car this-syntax)
         ;  #,
         (template
              (begin
                (struct name (?? parent) (field ...)
                  (?? transparent)
                  methods+props ...
                  (?? prefab))
                (define name/c
                  (struct/c name
                            (?? (?@ parent-contract ...))
                            contract ...))
                (module+ test
                  (require rackunit)
                  (check-pred flat-contract? name/c))
                (provide name/c
                         (contract-out (struct (?? (name parent) name)
                                         ((?? (?@ [parent-field parent-contract]
                                                  ...))
                                          [field contract]
                                          ...)))))))

       ;<hash-set/c>
       <printer>

       <field-info>
       <node-info>
       <invariant-info>
       <dependent-invariant-info>
       <graph-info>
       <mapping-info>
       <graph-builder-info>]
