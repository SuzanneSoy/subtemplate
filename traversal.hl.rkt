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
 @racket[syntax/parse/experimental/template] to indicate that the function
 accepts a predicate, followed by an update function, followed by another
 predicate, and so on. For example, the function type when there are three
 @racket[type-to-replaceᵢ] would be:

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


* free-identifier-tree=?
* cache of already-seen types
* recursively go down the tree. If there are no replacements, return #f all the
way up, so that a simple identity function can be applied in these cases.

@chunk[<define-fold>
       (define-syntax define-fold
         (syntax-parser
           [(_ function-name:id
               type-name:id
               whole-type:type
               type-to-replaceᵢ:type …)
            <define-fold-prepare>
            (template
             (begin
               <define-fold-result>))]))]

@chunk[<define-fold-prepare>
       (define-temp-ids "Tᵢ" (type-to-replaceᵢ …))
       (define-temp-ids "Aᵢ" (type-to-replaceᵢ …))
       (define-temp-ids "Bᵢ" (type-to-replaceᵢ …))
       (define-temp-ids "predicateᵢ" (type-to-replaceᵢ …))
       (define-temp-ids "updateᵢ" (type-to-replaceᵢ …))]

@chunk[<define-fold-prepare>
       (define/with-syntax (the-type the-code the-defs …)
         (syntax-parse #'whole-type
           #:literals (Null Pairof Listof List Vectorof Vector)
           [Null #'(Null (values v acc))]
           [(Pairof X Y)
            #'(Null
               (values v acc)
               (define-fold fx tx X type-to-replaceᵢ …)
               (define-fold fy ty Y type-to-replaceᵢ …))]
           [#t #'((Pairof Any Any) (void))]))]

@chunk[<define-fold-result>
       the-defs …

       (define-type (type-name Tᵢ …) the-type)

       (: function-name (∀ (Aᵢ … Bᵢ … Acc)
                           (→ (?@ (→ Any Boolean : Aᵢ)
                                  (→ Aᵢ Acc (Values Bᵢ Acc)))
                              …
                              (→ (type-name Aᵢ …)
                                 Acc
                                 (Values (type-name Bᵢ …)
                                         Acc)))))
       (define ((function-name (?@ predicateᵢ updateᵢ) …) v acc)
         the-code)]

@section{Putting it all together}

@chunk[<*>
       (require phc-toolkit
                (for-syntax racket/base
                            phc-toolkit/untyped
                            racket/syntax
                            syntax/parse
                            syntax/parse/experimental/template
                            type-expander/expander))

       (provide define-fold)
       <define-fold>]