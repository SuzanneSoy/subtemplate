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

@title{Parametric replacement of parts of data structures, identified by their
 type}

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

@defform[(define-fold name whole-type
           [type-to-replaceᵢ predicateᵢ] ...)]{
 The @racket[define-fold] macro takes the type of the whole data structure, and
 a list of types to replace, each associated with a predicate for that type. It
 defines @racket[_name] as a macro, which behaves as follows:

 @(make-blockquote
   "leftindent"
   (flow-paragraphs
    (decode-flow
     (splice-run
      @defform[#:link-target? #f (_name function-name
                                        new-whole-type-name
                                        accumulator-type
                                        [new-typeᵢ update-functionᵢ] ...)]{
  Each @racket[update-functionᵢ] must have the type:

  @racketblock[(→ accumulator-type
                  type-to-replaceᵢ
                  (values accumulator-type new-typeᵢ))]
                                                                           
  This macro defines @racket[new-whole-type-name] as a type similar to
  @racket[whole-type], but with each syntactic occurrence of a
  @racket[type-to-replaceᵢ] replaced with the corresponding @racket[new-typeᵢ].

  This macro also defines @racket[function-name] as a function with the type:

  @racketblock[(→ accumulator-type
                  whole-type
                  (values accumulator-type new-whole-type-name))]

  The @racket[function-name] replaces all values in the whole data structure
  which are present in locations corresponding to a @racket[type-to-replaceᵢ].
  Each value is passed as an argument to the corresponding
  @racket[update-functionᵢ], and the result is used as a replacement.

  An accumulator value, with the type @racket[accumulator-type], is threaded
  through all calls to all @racket[update-functionᵢ], so that the update
  functions can share state in a functional way.}))))}


@chunk[<define-fold>
       ]

@section{Putting it all together}

@chunk[<*>
       ]