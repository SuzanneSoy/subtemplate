#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
@(require racket/require
          scribble-enhanced/doc
          racket/require
          hyper-literate
          (subtract-in scribble/struct scribble-enhanced/doc)
          scribble/decode
          (for-label racket/format
                     racket/promise
                     racket/list
                     syntax/parse
                     syntax/parse/experimental/template
                     type-expander
                     (except-in (subtract-in typed/racket/base type-expander)
                                values)
                     (only-in racket/base values)
                     (subtract-in racket/contract typed/racket/base)
                     phc-toolkit
                     phc-toolkit/untyped-only
                     remember))
@(unless-preexpanding
  (require (for-label (submod ".."))))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "traversal"
       #:tag-prefix "phc-graph/traversal"]{Parametric replacement of parts of
 data structures, identified by their type}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/traversal"))

@(table-of-contents)

@(declare-exporting (lib "phc-graph/traversal.hl.rkt"))

@section{Introduction}

This utility allows functionally updating parts of data structures. The
@racket[define-fold] macro takes the type of the whole data structure and a
list of type names associated with their predicate. It locates all literal
occurrences of those type names within the data structure, and identifies
those locations as the parts to replace. The type of the whole data structure
is expressed as a syntactic tree. Within that syntactic tree, only the parts
which are syntactically equal to one of the types to replace are considered.

As an example, suppose the whole type is
@racket[(List Foo Number (Listof String))], and @racket[Foo] is defined as:

@racketblock[(define-type Foo (Listof String))]

If @racket[Foo] is given as a type to replace, and its replacement type is
@racket[(Listof Symbol)], then the type of the result would be:

@racketblock[(List (Listof Symbol) Number (Listof String))]

The second occurrence of @racket[(Listof String)], although semantically
equivalent to the type to replace, @racket[Foo], will not be altered, as it is
not expressed syntactically using the @racket[Foo] identifier.

@defform[(define-fold function-name type-name whole-type type-to-replaceᵢ ...)]{
 The @racket[define-fold] macro takes the type of the whole data structure, and
 a list of types to replace, each associated with a predicate for that type. It
 @;defines @racket[_name] as a macro, which behaves as follows:
 defines @racket[(type-name Tᵢ ...)] as a polymorphic type, with one type
 argument for each @racket[type-to-replaceᵢ], such that

 @racketblock[(type-name type-to-replaceᵢ ...)]

 is the same type as

 @racketblock[whole-type]

 In other words, @racket[type-name] is defined as @racket[whole-type], except
 that each syntactic occurrence of a @racket[type-to-replaceᵢ] is replaced with
 the corresponding type argument @racket[Tᵢ].

 It also defines @racket[function-name] as a function, with the type

 @racketblock[(∀ (Aᵢ ... Bᵢ ... Acc)
                 (→ (?@ (→ Any Boolean : Aᵢ)
                        (→ Aᵢ Acc (Values Bᵢ Acc)))
                    ...
                    (→ (type-name Aᵢ ...)
                       Acc
                       (Values (type-name Bᵢ ...)
                               Acc))))]

 We use the @racket[?@] notation from
 @racketmodname[syntax/parse/experimental/template] to indicate that the
 function accepts a predicate, followed by an update function, followed by
 another predicate, and so on. For example, the function type when there are
 three @racket[type-to-replaceᵢ] would be:

 @racketblock[(∀ (A₁ A₂ A₃ B₁ B₂ B₃ Acc)
                 (→ (→ Any Boolean : A₁)
                    (→ A₁ Acc (Values B₁ Acc))
                    (→ Any Boolean : A₂)
                    (→ A₂ Acc (Values B₂ Acc))
                    (→ Any Boolean : A₃)
                    (→ A₃ Acc (Values B₃ Acc))
                    (→ (type-name A₁ A₂ A₃)
                       Acc
                       (Values (type-name B₁ B₂ B₃)
                               Acc))))]

 The @racket[function-name] replaces all values in the whole data structure
 which are present in locations corresponding to a @racket[type-to-replaceᵢ] in
 the @racket[whole-type]. It expects those values to have the type @racket[Aᵢ],
 i.e. its input type is not restricted to @racket[whole-type], any polymorphic
 instance of @racket[type-name] is valid. Each value is passed as an argument
 to the corresponding update function with type
 @racket[(→ Aᵢ Acc (Values Bᵢ Acc))], and the result of type @racket[Bᵢ] is
 used as a replacement.
 
 An accumulator value, with the type @racket[Acc], is threaded through all
 calls to all update functions, so that the update functions can communicate
 state in a functional way.}

@section{Implementation}

@subsection{Caching the results of @racket[define-fold]}

@chunk[<with-folds>
       (define-for-syntax get-with-folds-cache (make-parameter #f))
       (define-syntax (with-folds stx)
         (syntax-case stx ()
           [(_ . body*)
            (parameterize ([get-with-folds-cache (mutable-hash)])
              (define expanded-body (local-expand #'(begin . body*)
                                                  (syntax-local-context)
                                                  '()))
              (define/with-syntax (cached-definition …)
                (append-map (λ (key cached)
                              (with-syntax ([(f-id τ-id f-body τ-body) def-ids])
                                (list #'(define-type τ-id τ-body)
                                      #'(define f-id f-body))))
                            (hash->list (get-with-folds-cache))))
              #`(begin cached-definition …
                       expanded-body))]))]

@;@subsection{…}


* free-id-tree=?
* cache of already-seen types
* recursively go down the tree. If there are no replacements, return #f all the
way up, so that a simple identity function can be applied in these cases.


@CHUNK[<define-fold>
       (define-type-expander (replace-in-type stx)
         (syntax-case stx ()
           [(_ _whole-type [_type-to-replaceᵢ _Tᵢ] …)
            ;+ cache
            #'((fold-type _whole-type _type-to-replaceᵢ …) _Tᵢ …)]))
       (define-type-expander fold-type
         (syntax-parser
           [(_ _whole-type:type _type-to-replaceᵢ:type …)
            #:with rec-args (subtemplate
                             ([_type-to-replaceᵢ _Tᵢ] …))
            (define replacements (make-immutable-free-id-tree-table
                                  (map syntax-e
                                       (syntax->list
                                        (subtemplate
                                         ([_type-to-replaceᵢ . _Tᵢ] …))))))
            #;(define-template-metafunction (rec-replace stx)
              (syntax-case stx ()
                [(_ τ) #'(replace-in-type τ . rec-args)]))
            ((λ (x) (displayln "τ=") (pretty-write (syntax->datum x)) x)
             (quasisubtemplate
              (∀ (_Tᵢ …)
                 #,(syntax-parse #'_whole-type
                       #:literals (Null Pairof Listof List Vectorof Vector U tagged)
                       <type-cases>))))]))]

@CHUNK[<define-fold>
       (define-syntax (replace-in-instance stx)
         (syntax-case stx ()
           [(_ _whole-type
               [_type-to-replaceᵢ _predicateᵢ _updateᵢ] …)
            ;+ cache
            (subtemplate
             ((fold-f _whole-type _type-to-replaceᵢ …)
              {?@ _predicateᵢ _updateᵢ} …))]))
       
       (define-syntax fold-f
         (syntax-parser
           [(_ _whole-type:type _type-to-replaceᵢ:type …)
            #:with rec-args (subtemplate
                             ([_type-to-replaceᵢ _predicateᵢ _updateᵢ] …))
            (define replacements (make-immutable-free-id-tree-table
                                  (map syntax-e
                                       (syntax->list
                                        (subtemplate
                                         ([_type-to-replaceᵢ . _updateᵢ] …))))))
            #;(define-template-metafunction (λrec-replace stx)
              (syntax-case stx ()
                [(_ τ)
                 #'(replace-in-instance τ . rec-args)]))
            #;(define-template-metafunction (rec-replace stx)
              (syntax-case stx ()
                [(_ τ v acc)
                 #'((replace-in-instance τ . rec-args) v acc)]))
            (define/with-syntax _args (subtemplate ({?@ _predicateᵢ _updateᵢ} …)))
            ((λ (x) (displayln "f=") (pretty-write (syntax->datum x)) x)
             (quasisubtemplate
              (ann (λ ({?@ _predicateᵢ _updateᵢ} …)
                     (λ (v acc)
                       #,(syntax-parse #'_whole-type
                             #:literals (Null Pairof Listof List Vectorof Vector U tagged)
                             <f-cases>)))
                   (∀ (_Aᵢ … _Bᵢ … Acc)
                      (→ (?@ (→ Any Boolean : _Aᵢ)
                             (→ _Aᵢ Acc (Values _Bᵢ Acc)))
                         …
                         (→ (replace-in-type _whole-type
                                             [_type-to-replaceᵢ _Aᵢ] …)
                            Acc
                            (Values (replace-in-type _whole-type
                                             [_type-to-replaceᵢ _Bᵢ] …)
                                    Acc)))))))]))]

@chunk[<f-cases>
       [t
        #:when (dict-has-key? replacements #'t)
        #:with _update (dict-ref replacements #'t)
        (subtemplate (_update v acc))]]

@chunk[<type-cases>
       [t
        #:when (dict-has-key? replacements #'t)
        #:with _T (dict-ref replacements #'t)
        (subtemplate _T)]]

@chunk[<type-cases>
       [(~or Null (List))
        (subtemplate Null)]]

@chunk[<f-cases>
       [(~or Null (List))
        (subtemplate (values v acc))]]


@chunk[<type-cases>
       [(Pairof X Y)
        (subtemplate (Pairof (replace-in-type X . rec-args)
                             (replace-in-type Y . rec-args)))]]

@chunk[<f-cases>
       [(Pairof X Y)
        (subtemplate
         (let*-values ([(result-x acc-x)
                        ((replace-in-instance X . rec-args) (car v) acc)]
                       [(result-y acc-y)
                        ((replace-in-instance Y . rec-args) (cdr v) acc-x)])
           (values (cons result-x result-y) acc-y)))]]

@chunk[<type-cases>
       [(Listof X)
        (subtemplate
         (Listof (replace-in-type X . rec-args)))]]

@chunk[<f-cases>
       [(Listof X)
        (subtemplate
         (foldl-map (replace-in-instance X . rec-args)
                    acc v))]]

@chunk[<type-cases>
       [(Vectorof X)
        (subtemplate
         ;; TODO: turn replace-in-type & co into rec-replace via metafunctions
         (Vectorof (replace-in-type X . rec-args)))]]

@chunk[<ftype-cases>
       [(Vectorof X)
        (subtemplate
         (vector->immutable-vector
          (list->vector
           (foldl-map (replace-in-instance X . rec-args) acc (vector->list v)))))]]


@chunk[<type-cases>
       [(List X Y …)
        (subtemplate
         (Pairof (replace-in-type X . rec-args)
                 (replace-in-type (List Y …) . rec-args)))]]

@chunk[<f-cases>
       [(List X Y …)
        (subtemplate
         (let*-values ([(result-x acc-x) ((replace-in-instance X . rec-args)
                                          (car v)
                                          acc)]
                       [(result-y* acc-y*) ((replace-in-instance (List Y …) . rec-args)
                                            (cdr v)
                                            acc-x)])
           (values (cons result-x result-y*) acc-y*)))]]

@chunk[<type-cases>
       [(U _Xⱼ …)
        (subtemplate
         (U (replace-in-type _Xⱼ . rec-args) …))]]

@chunk[<f-cases>
       [(U _Xⱼ …)
        (subtemplate
         (dispatch-union v
                         ([_type-to-replaceᵢ Aᵢ _predicateᵢ] …)
                         [_Xⱼ ((replace-in-instance _Xⱼ . rec-args) v acc)] …))]]

@chunk[<type-cases>
       [(tagged _name [_fieldⱼ (~optional :colon) _Xⱼ] …)
        (subtemplate
         (tagged _name [_fieldⱼ : (replace-in-type _Xⱼ . rec-args)] …))]]

@chunk[<f-cases>
       [(tagged _name [_fieldⱼ (~optional :colon) _Xⱼ] …)
        (subtemplate
         (let*-values ([(_resultⱼ acc)
                        ((replace-in-instance _Xⱼ . rec-args) (uniform-get v _fieldⱼ)
                                                              acc)]
                       …)
           (values (tagged _name #:instance [_fieldⱼ _resultⱼ] …)
                   acc)))]]

@chunk[<type-cases>
       [else-T
        (subtemplate
         else-T)]]

@chunk[<f-cases>
       [else-T
        (subtemplate
         (values v acc))]]



------



@chunk[<define-fold>
       (define-syntax define-fold
         (syntax-parser
           [(_ _function-name:id
               _type-name:id
               whole-type:type
               _type-to-replaceᵢ:type …)
            #'(begin
                (define-type _type-name
                  (fold-type whole-type _type-to-replaceᵢ …))
                (define _function-name
                  (fold-f whole-type _type-to-replaceᵢ …)))]))
       #;(define-syntax define-fold
           (syntax-parser
             [(_ _function-name:id
                 _type-name:id
                 whole-type:type
                 _type-to-replaceᵢ:type …)
              <define-fold-prepare>
              ((λ (x)
                 (local-require racket/pretty)
                 #;(pretty-write (syntax->datum x))
                 x)
               (subtemplate
                (begin
                  <define-fold-result>)))]))]

@chunk[<define-fold-result>
       the-defs …

       (define-type (_type-name _Tᵢ …) _the-type)

       (: _function-name (∀ (_Aᵢ … _Bᵢ … Acc)
                            (→ (?@ (→ Any Boolean : _Aᵢ)
                                   (→ _Aᵢ Acc (Values _Bᵢ Acc)))
                               …
                               (→ (_type-name _Aᵢ …)
                                  Acc
                                  (Values (_type-name _Bᵢ …)
                                          Acc)))))
       (define ((_function-name . _args) v acc)
         _the-code)]

@chunk[<define-fold-prepare>
       (define/with-syntax _args (subtemplate ({?@ _predicateᵢ _updateᵢ} …)))]

@chunk[<define-fold-prepare>
       (type-cases
        syntax-parse
        (whole-type #:to _the-type
                    #:using _the-code
                    #:with-defintitions the-defs …)
        #:literals (Null Pairof Listof List Vectorof Vector U tagged)
        <old-type-cases>)]

@chunk[<old-type-cases>
       [t
        #:with (_ update T)
        (findf (λ (r) (free-id-tree=? #'t (stx-car r)))
               (syntax->list (subtemplate ([_type-to-replaceᵢ _updateᵢ _Tᵢ] …))))
        
        #:to
        T

        #:using
        (update v acc)]]

@chunk[<old-type-cases>
       [(~or Null (List))

        #:to
        Null

        #:using
        (values v acc)]]

@chunk[<old-type-cases>
       [(Pairof X Y)

        #:to
        (Pairof (tx _Tᵢ …) (ty _Tᵢ …))

        #:using
        (let*-values ([(result-x acc-x) ((fx . _args) (car v) acc)]
                      [(result-y acc-y) ((fy . _args) (cdr v) acc-x)])
          (values (cons result-x result-y) acc-y))

        #:with-defintitions
        (define-fold fx tx X _type-to-replaceᵢ …)
        (define-fold fy ty Y _type-to-replaceᵢ …)]]

@chunk[<old-type-cases>
       [(Listof X)

        #:to
        (Listof (te _Tᵢ …))

        #:using
        (foldl-map (fe . _args) acc v)

        #:with-defintitions
        (define-fold fe te X _type-to-replaceᵢ …)]]

@chunk[<old-type-cases>
       [(Vectorof X)

        #:to
        (Vectorof (te _Tᵢ …))

        #:using
        (vector->immutable-vector
         (list->vector
          (foldl-map (fe . _args) acc (vector->list v))))

        #:with-defintitions
        (define-fold fe te X _type-to-replaceᵢ …)]]

@chunk[<old-type-cases>
       [(List X Y …)

        #:to
        (Pairof (tx _Tᵢ …) (ty* _Tᵢ …))

        #:using
        (let*-values ([(result-x acc-x) ((fx . _args) (car v) acc)]
                      [(result-y* acc-y*) ((fy* . _args) (cdr v) acc-x)])
          (values (cons result-x result-y*) acc-y*))

        #:with-defintitions
        (define-fold fx tx X _type-to-replaceᵢ …)
        (define-fold fy* ty* (List Y …) _type-to-replaceᵢ …)]]

@chunk[<old-type-cases>
       [(U _Xⱼ …)

        #:to
        (U (_txⱼ _Tᵢ …) …)

        #:using
        (dispatch-union v
                        ([_type-to-replaceᵢ Aᵢ _predicateᵢ] …)
                        [_Xⱼ ((_fxⱼ . _args) v acc)] …)

        #:with-defintitions
        (define-fold _fxⱼ _txⱼ _Xⱼ _type-to-replaceᵢ …)
        …]]

@chunk[<old-type-cases>
       [(tagged _name [_fieldⱼ (~optional :colon) _Xⱼ] …)

        #:to
        (tagged _name [_fieldⱼ : (_txⱼ _Tᵢ …)] …)

        #:using
        (let*-values ([(_resultⱼ acc) ((_fxⱼ . _args) (uniform-get v _fieldⱼ)
                                                      acc)]
                      …)
          (values (tagged _name #:instance [_fieldⱼ _resultⱼ] …)
                  acc))

        #:with-defintitions
        (define-fold _fxⱼ _txⱼ _Xⱼ _type-to-replaceᵢ …)
        …]]

@chunk[<old-type-cases>
       [else-T

        #:to
        else-T

        #:using
        (values v acc)]]

where @racket[foldl-map] is defined as:

@chunk[<foldl-map>
       (: foldl-map (∀ (A B Acc) (→ (→ A Acc (Values B Acc))
                                    Acc
                                    (Listof A)
                                    (Values (Listof B) Acc))))
       (define (foldl-map f acc l)
         (if (null? l)
             (values l
                     acc)
             (let*-values ([(v a) (f (car l) acc)]
                           [(ll aa) (foldl-map f a (cdr l))])
               (values (cons v ll)
                       aa))))]

@chunk[<type-cases-macro>
       (define-syntax type-cases
         (syntax-parser
           [(_ sp
               (whole-type #:to the-type
                           #:using the-code
                           #:with-defintitions the-defs (~literal …))
               #:literals (lit …)
               (pat opts …
                    #:to transform-type
                    #:using transform-code
                    (~optional (~seq #:with-defintitions transform-defs …)
                               #:defaults ([(transform-defs 1) (list)])))
               …)
            #'(define/with-syntax (the-type the-code the-defs (… …))
                (sp #'whole-type
                    #:literals (lit …)
                    [pat opts …
                     (subtemplate
                      (transform-type transform-code transform-defs …))]
                    …))]))]


@section{Putting it all together}

@chunk[<*>
       (require racket/require
                phc-toolkit
                type-expander
                phc-adt
                "dispatch-union.rkt"
                (for-syntax "subtemplate.rkt"
                            (subtract-in racket/base "subtemplate.rkt")
                            phc-toolkit/untyped
                            racket/syntax
                            (subtract-in syntax/parse "subtemplate.rkt")
                            syntax/parse/experimental/template
                            type-expander/expander
                            "free-identifier-tree-equal.rkt"
                            racket/dict
                            racket/pretty)
                (for-meta 2 racket/base)
                (for-meta 2 phc-toolkit/untyped)
                (for-meta 2 syntax/parse)
                racket/pretty)

       (provide define-fold
                replace-in-instance
                replace-in-type)
       (begin-for-syntax <type-cases-macro>)
       <foldl-map>
       <define-fold>]