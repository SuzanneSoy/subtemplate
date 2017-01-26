#lang scribble/manual
@require[@for-label[subtemplate
                    syntax/parse/experimental/template
                    racket/base]]

@title{Subtemplate}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[subtemplate]

@defform*[{(subtemplate template)
           (subtemplate template #:properties (prop ...))}
         #:contracts
         ([prop identifier?])]{
 Like @racket[template], but automatically derives identifiers for any
 @racket[yᵢ …] which is not bound as a syntax pattern variable, based on a
 corresponding @racket[xᵢ …] which is bound as a syntax pattern variable.

 Note that the syntax pattern variables must be matched with one of the
 patched forms from @racket[stxparse-info/parse] or
 @racket[stxparse-info/case], instead of the syntax pattern-matching forms from
 @racket[syntax/parse] or @racket[racket/base], respectively.}

@defform*[{(quasisubtemplate template)
           (quasisubtemplate template #:properties (prop ...))}
         #:contracts
         ([prop identifier?])]{
 Like @racket[quasitemplate], but automatically derives identifiers for any
 @racket[yᵢ …] which is not bound as a syntax pattern variable, based on a
 corresponding @racket[xᵢ …] which is bound as a syntax pattern variable, in
 the same way as @racket[subtemplate].

 Note that the syntax pattern variables must be matched with one of the
 patched forms from @racket[stxparse-info/parse] or
 @racket[stxparse-info/case], instead of the syntax pattern-matching forms from
 @racket[syntax/parse] or @racket[racket/base], respectively.
}

@section{Overriding the default @racket[#'…] and @racket[#`…]}

@defmodule[subtemplate/override]

The @racketmodname[subtemplate/override] module re-provides
@racket[subtemplate] as @racket[syntax], and @racket[quasisubtemplate] as
@racket[quasisyntax]. This allows @racketmodname[subtemplate] to be used via
the reader shorthands @racket[#'…] and @racket[#`…].

The @racketmodname[subtemplate/override] module also re-provides
@racketmodname[stxparse-info/parse] and @racketmodname[stxparse-info/case].