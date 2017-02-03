#lang scribble/manual
@require[scriblib/footnote
         @for-label[subtemplate/light
                    syntax/parse/experimental/template
                    racket/base]]

@(begin
   (module m racket/base
     (require scribble/manual
              (for-template subtemplate)
              (for-syntax racket/base
                          racket/syntax))
     (define-syntax (mk stx)
       (syntax-case stx ()
         [(_ id)
          (with-syntax ([full: (format-id #'id "full:~a" #'id)])
            #'(begin
                (define full: @racket[id])
                (provide full:)))]))
     (define-syntax-rule (mk* id ...) (begin (mk id) ...))
     (mk* subtemplate ?@@ ?attr ?cond ?if))
   (require 'm))

@title{Lightweight Subtemplate}

@defmodule[subtemplate/light]{

 This module only provides stripped-down versions of @racket[subtemplate] and
 @racket[quasisubtemplate], without overriding @racket[syntax] and
 @racket[quasisyntax]. Note that some features will not work when using these
 versions. Prefer using @racket[(require subtemplate)] instead.

 Another limitation is that subscripted identifiers are not searched for
 within unquoted parts of the template.

 Note that you need to require @racketmodname[stxparse-info/parse] and
 @racketmodname[stxparse-info/case], otherwise @racket[subtemplate] and
 @racket[quasisubtemplate] will not be able to detect which pattern variables
 are bound (and therefore will be unable to know from which @racket[xᵢ] an
 @racket[yᵢ] should be derived.}

@defform*[{(subtemplate template)
           (subtemplate template #:properties (prop ...))}
          #:contracts
          ([prop identifier?])]{

 Like @full:subtemplate from @racketmodname[subtemplate], but with a few
 features missing (@full:?@@ @full:?attr @full:?cond @full:?if).}

@defform*[{(subtemplate template)
           (subtemplate template #:properties (prop ...))}
          #:contracts
          ([prop identifier?])]{

 Like @full:subtemplate from @racketmodname[subtemplate], but with a few
 features missing. The utilities @full:?@@ @full:?attr @full:?cond @full:?if
 are not taken into account, and @racket[unsyntax] completely escapes the
 ellipses.
 
 Note that the syntax pattern variables must be matched with one of the
 patched forms from @racketmodname[stxparse-info/parse] or
 @racketmodname[stxparse-info/case], instead of the syntax pattern-matching
 forms from @racketmodname[syntax/parse] or @racketmodname[racket/base],
 respectively.}

@defform*[{(quasisubtemplate template)
           (quasisubtemplate template #:properties (prop ...))}
          #:contracts
          ([prop identifier?])]{

 Like @full:subtemplate from @racketmodname[subtemplate], but with a few
 features missing. The utilities @full:?@@ @full:?attr @full:?cond @full:?if
 are not taken into account, and @racket[unsyntax] completely escapes the
 ellipses

 Another limitation is that subscripted identifiers are not searched for
 within unquoted parts of the template.

 Note that the syntax pattern variables must be matched with one of the
 patched forms from @racketmodname[stxparse-info/parse] or
 @racketmodname[stxparse-info/case], instead of the syntax pattern-matching
 forms from @racketmodname[syntax/parse] or @racketmodname[racket/base],
 respectively. }
