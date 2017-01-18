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

@chunk[<graph-info>
       (struct+/contract graph-info
         ([name identifier?]
          [tvars (listof identifier?)]
          [root-node identifier?]
          [node-order (listof identifier?)]
          [nodes (hash/c symbol? node-info? #:immutable #t)]
          [invariants (equal-hash-set/c invariant-info? #:kind 'immutable)])
         #:prefab)]

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
             ;; This is redundant as the mutable check is already included above
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

@section{Graph builder information}

@chunk[<graph-builder-info>
       (struct+/contract graph-builder-info graph-info
         ([name identifier?]
          [tvars (listof identifier?)]
          [root-node identifier?]
          [node-order (listof identifier?)]
          [nodes (hash/c symbol? node-info? #:immutable #t)]
          [invariants (equal-hash-set/c invariant-info? #:kind 'immutable)])
         ([multi-constructor identifier?]
          [root-mapping identifier?]
          [mapping-order (listof identifier?)]
          [mappings (hash/c symbol? mapping-info? #:immutable #t)]
          [dependent-invariants (equal-hash-set/c dependent-invariant-info?
                                                  #:kind 'immutable)])
         #:prefab)]

@section{Node information}

@chunk[<node-info>
       (struct+/contract node-info
         ([predicate? identifier?]
          [field-order (listof identifier?)]
          [fields (hash/c symbol? field-info? #:immutable #t)]
          [promise-type identifier?]
          [make-incomplete-type identifier?]
          [incomplete-type identifier?])
         #:prefab)]

@section{Field information}

A field has a type.

@chunk[<field-info>
       (struct+/contract field-info
         ([type identifier?])
         #:prefab)]

@;[incomplete-type identifier?]

@section{Invariant information}

@chunk[<invariant-info>
       (struct+/contract invariant-info
         ([predicate identifier?] ; (→ RootNode Boolean : +witness-type)
          [witness-type identifier?])
         #:prefab)]

@section{Dependent invariant information}

The invariants described in the previous section assert properties of a graph
instance in isolation. It is however desirable to also describe invariants
which relate the old and the new graph in a graph transformation.

@chunk[<dependent-invariant-info>
       (struct+/contract dependent-invariant-info
         ([checker identifier?] ; (→ RootMappingArguments… NewGraphRoot Boolean)
          [name identifier?])
         #:prefab)]

@section{Mapping information}

@chunk[<mapping-info>
       (struct+/contract mapping-info
         ([mapping-function identifier?]
          [with-promises-type identifier?]
          [make-placeholder-type identifier?]
          [placeholder-type identifier?])
         #:prefab)]

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
         (quasisyntax/top-loc this-syntax
           #,(template
              (begin
                (struct name (?? parent) (field ...)
                  (?? transparent)
                  methods+props ...
                  (?? prefab))
                (define name/c
                  (struct/c name
                            (?? (?@ parent-contract ...))
                            contract ...))
                (provide name/c
                         (contract-out (struct (?? (name parent) name)
                                         ((?? (?@ [parent-field parent-contract]
                                                  ...))
                                          [field contract]
                                          ...))))))))

       <hash-set/c>
       <printer>

       <field-info>
       <node-info>
       <invariant-info>
       <dependent-invariant-info>
       <graph-info>
       <mapping-info>
       <graph-builder-info>]
