#lang scribble/manual
@require[racket/require
         scriblib/footnote
         scribble-math
         "orig.rkt"
         @for-label[subtemplate
                    (only-in syntax/parse/experimental/template)
                    (subtract-in racket/base subtemplate)]]

@title[#:style (with-html5 manual-doc-style)]{Subtemplate}
@author[@author+email["Suzanne Soy" "racket@suzanne.soy"]]

This library should be considered experimental. Although most of the syntax
should work in the same way in future versions, the behaviour of some corner
cases may change, as I try to find the best semantics.

Also, this library requires some patched versions of @racket[syntax-parse] and
@orig:syntax-case, as these do not offer the hooks needed to implement
@racket[subtemplate]. Unfortunately, as the official implementations of
@racket[syntax-parse] and @racket[syntax-case] evolve, compatibility issues
may arise.

If you desire to use this library, please drop me an e-mail (my address is
below the title), so that I can keep you informed of upcoming changes, see if
these are likely to cause problems in your code.

Finally, If the maintenance burden is too high, I might drop the compatibility
with @racketmodname[syntax/parse] and @|orig:syntax-case|.

@include-section{examples.scrbl}

@section{The main @racketmodname[subtemplate] module}

@defmodule[subtemplate]{                        
 The @racketmodname[subtemplate] module provides @racket[subtemplate], an
 alternative to @racketmodname[syntax/parse]'s @orig:template, which
 supports several convenience features:

 @itemlist[
 @item{When an identifier @racket[yᵢ] is encountered in a template, it is
   automatically defined as a pattern variable containing temporary identifiers.

   This avoids the need to manually call @racket[generate-temporaries].

   The generated temporary identifiers will be based on a @racket[xᵢ] pattern
   variable with the same subscript as @racket[yᵢ], and @racket[yᵢ] will be
   nested at the same ellipsis depth as @racket[xᵢ].

   The identifiers @racket[xᵢ] and @racket[yᵢ] must end wit the same subscript,
   which must be a sequence of unicode subscript characters picked among
   @tt{ₐ ₑ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵣ ₛ ₜ ᵤ ᵥ ₓ ᵦ ᵧ ᵨ ᵩ ᵪ}. Alternatively, the
   subscript may be specified with an underscore followed by any characters
   other than an underscore. The two notations are equivalent, in the sense that
   @racket[yᵦ] and @racket[y_β] are interpreted in the same way, and if both
   appear within a template, they will use the same sequence of temporary
   identifiers generated from an @racket[xᵦ] or @racket[x_β].}
 @item{The value of pattern variables is automatically extracted when the
   variable does not appear in a syntax template. Note that since the syntax
   object is transformed into a plain datum, source locations and lexical
   contexts are lost. This is a trade-off between better error messages, which
   make sure that source locations and lexical context are not lost by accident
   (no automatic @racket[syntax-e]), and more concise code (with automatic
   @racket[syntax-e]). It is possible that a future version may require explicit
   syntax-e, possibly via a concise shorthand like @racket[unquote] (@tt{,}) or
   @racket[unsyntax] (@tt{#,}), if this feature turns out to be too dangerous in
   practice.}
 @item{Ellipses work outside of syntax templates, and can be used after
   definitions and expressions.
   @itemlist[
 @item{The result of an expression under @${n} ellipses is a
     @${\text{nested}^n} list, where the expression is evaluated for
     each value of the pattern variables located within. In other words,
     @racket[(x ...)] should produce a value similar to
     @racket[(syntax->datum #'(x ...))]. However, it is possible to actually
     manipulate the value, e.g. by writing @racket[(+ x 1) ...]. It is possible
     to write @${m} ellipses in a row (which has the effect of flattening
     @${m - 1} levels in the result list). It is also possible to nest the use
     of these ellipses, e.g. with @racket[(x ...) ...], which keeps the
     structure of the nested lists in the result.}
 @item{When a definition form (@racket[define], @racket[define/with-syntax] or
     @racket[define/syntax-parse] for now) is followed by @${n} ellipses, then
     the defined identifier is a @${\text{nested}^n} list, or a syntax pattern
     variable with an ellipsis depth of @${n}. The expression is evaluated for
     each value of the template variables it contains. Note that the structure
     of the nested lists is not flattened, despite the fact that the ellipses
     are written one after another. This is because it is usually the desired
     outcome, and nesting parentheses around the definition form would produce
     rather unreadable code.}
 @item{These ellipses can also be used ``inline'' within function calls
     (@racketmodname[subtemplate] overrides @racket[#%app] to achieve this). For
     example: @racket[(/ (+ x ...) (length x))] would compute the average of
     @racket[(syntax->datum #'(x ...))]}
 @item{Subscripted identifiers should also work in expressions
     (@racketmodname[subtemplate] overrides @racket[#%top] to achieve this),
     although this seems less useful, as the temporary identifiers loose their
     lexical context information in that way.}
 @item{The splicing forms @racket[?@] and @racket[?@@], as well as
     @racket[??], @racket[?if] and @racket[?cond] work within expressions, and
     can be used to splice values into the argument list when calling a
     function. For example, @racket[(+ 1 (?@ l) 3)] would splice the values of
     the list @racket[l] into the argument list, effectively calling
     @racket[(+ 1 97 98 99 3)] if @racket[l] was equal to
     @racket[#'(97 98 99)]. Additionnally, it is possible to append a list
     at the end of the argument list, with the syntax @racket[(+ 1 2 . l)].}
 @item{It is possible to create a syntax object based on one of the iterated
     pattern variables within the expression-ellipses, for example using
     @racket[(#'x ...)].}]}
 @item{Within a @racket[subtemplate] and a @racket[quasisubtemplate], it is
   possible to use @racket[unsyntax] and @racket[unsyntax-splicing] to escape
   from the template. Within the escaped expression, the ellipsis depth of the
   template is conserved, making it possible to write
   @RACKET[(subtemplate (#,(+ x 1) ...))], for example.

   The usual behaviour, which resets the ellipsis count to 0, can be obtained
   with @RACKET[#,,expr] (that is,
   @racket[(#,(racket unsyntax) (#,(racket unquote) expr))]) for an
   @racket[unsyntax]-like escape. An @racket[unsyntax-splicing]-style escape can
   be obtained with @RACKET[#,,@expr] or @RACKET[#,@,expr] (that is,
   @racket[(#,(racket unsyntax) (#,(racket unquote-splicing) expr))] or
   @racket[(#,(racket unsyntax-splicing) (#,(racket unquote) expr))]).}
 @item{Several utilities in the spirit of @racket[??] and @racket[?@] are
   provided, namely @racket[?@@], @racket[?attr] @racket[?cond] and
   @racket[?if].}
 @item{All features (subscripted identifiers, dotted expressions and
   definitions, and the ellipsis-preserving @racket[unsyntax]) should work well
   with omitted elements in attributes, as created by
   @racket[~optional] or @racket[~or] in @racket[syntax-parse].}]}

@subsection{Modules re-provided by @racketmodname[subtemplate]}

The @racketmodname[subtemplate] library needs some cooperation from
@racket[syntax-case], @racket[syntax-parse] and similar forms. For this
reason, some patched versions are defined in the @racketmodname[stxparse-info]
library. @racket[subtemplate] cannot work properly if the right modules are
loaded. To make it easier to use @racketmodname[subtemplate], it re-provides
the modules that need to be loaded for it to function properly.

The @racketmodname[subtemplate] module re-provides
@racketmodname[stxparse-info/parse], @racketmodname[stxparse-info/case] and
the parts of @racketmodname[racket/syntax] which are not overridden by
@racketmodname[stxparse-info/case].

The @racketmodname[subtemplate/private/override] module also re-provides
@racketmodname[stxparse-info/parse/experimental/template], but without
@orig:template, @orig:quasitemplate, @orig:?? and @orig:?@, which are remapped
to their equivalents from this library, and without @orig:template/loc] and
@orig:quasitemplate/loc, which do not have an equivalent yet.

@subsection{New and overridden bindings provided by @racketmodname[subtemplate]}

@defform*[{(subtemplate tmpl)
           (subtemplate tmpl #:properties (prop ...))}
          #:contracts
          ([prop identifier?])]{
                                
 Like @orig:template from @racketmodname[syntax/parse/experimental/template],
 but automatically derives identifiers for any @racket[yᵢ …] which is not bound
 as a syntax pattern variable, based on a corresponding @racket[xᵢ …] which is
 bound as a syntax pattern variable. Additionally, @racket[subtemplate]
 supports a number of features described in
 @secref["The_main_subtemplate_module"
         #:doc '(lib "subtemplate/scribblings/subtemplate.scrbl")],
 which are not part of @racketmodname[syntax/parse/experimental/template]}

@defform*[{(quasisubtemplate tmpl)
           (quasisubtemplate tmpl #:properties (prop ...))}
          #:contracts
          ([prop identifier?])]{
 Like @orig:quasitemplate from
 @racketmodname[syntax/parse/experimental/template], but automatically derives
 identifiers for any @racket[yᵢ …] which is not bound as a syntax pattern
 variable, based on a corresponding @racket[xᵢ …] which is bound as a syntax
 pattern variable, in the same way as @racket[subtemplate]. Additionally,
 @racket[quasisubtemplate] supports a number of features described in
 @secref["The_main_subtemplate_module"
         #:doc '(lib "subtemplate/scribblings/subtemplate.scrbl")],
 which are not part of @racketmodname[syntax/parse/experimental/template]}

@defform*[{(template tmpl)
          (template tmpl #:properties (prop ...))}
         #:contracts
         ([prop identifier?])]{
                                
 Like @racket[subtemplate], but does not automatically generate pattern
 variables based on their subscript. The other features still work
 (ellipsis-preserving escapes with @racket[unsyntax], support for @racket[?@@],
 @racket[?attr], @racket[?cond] and @racket[?if]).}

@defform*[{(quasitemplate tmpl)
          (quasitemplate tmpl #:properties (prop ...))}
         #:contracts
         ([prop identifier?])]{
                                
 Like @racket[quasisubtemplate], but does not automatically generate pattern
 variables based on their subscript. The other features still work
 (ellipsis-preserving escapes with @racket[unsyntax], support for @racket[?@@],
 @racket[?attr], @racket[?cond] and @racket[?if]).}

@defform[#:kind "procedure"
         (?@ . expr)]{Splices the @racket[expr] into the surrounding form
 (which must be a function application). If the surrounding form is a
 @racket[begin], @racket[let], or @racket[#%intdef-begin], then the the
 splicing lists are not processed, but may be processed later by using the
 splicing-list value as an argument to a function.

 Also works in @racket[template], @racket[subtemplate] and their derivatives.}
@defform[#:kind "procedure"
         (?@@ . expr)]{Appends all the lists contained within @racket[expr],
 and splices the resulting list into the surrounding form. If the
 surrounding form is a @racket[begin], @racket[let], or
 @racket[#%intdef-begin], then the splicing lists are not processed, but may be
 processed later by using the splicing-list value as an argument to a
 function.

 Also works in @racket[template], @racket[subtemplate] and their derivatives.}
@defform*[[(?? alt)
           (?? alt ...+ else)]]{
 Executes @racket[alt], if none of the template variables within is omitted
 (i.e. bound to @racket[#false] for the current ellipsis iteration). Otherwise,
 the next @racket[alt] is considered. If every @racket[alt] contains omitted
 template variables, then @racket[else] is excuted. If only one @racket[alt] is
 specified, without an @racket[else], then @racket[else] defaults to
 @racket[(?@)], i.e. the empty splice.

 Also works in @racket[template], @racket[subtemplate] and their derivatives.}

@defform*[[(?if condition alt)
           (?if condition alt else)]]{
 Generalisation of @racket[??]. If none of the template variables within
 @racket[condition] is omitted (i.e. bound to @racket[#false] for the current
 ellipsis iteration), then @racket[alt] is executed. Otherwise, @racket[else]
 is executed.

 Also works in @racket[template], @racket[subtemplate] and their derivatives.}

@defform[(?attr condition)]{Shorthand for @racket[?if condition #t #f]

 Also works in @racket[template], @racket[subtemplate] and their derivatives.}

@defform*[#:literals (else)
          [(?cond [condition alt] …)
           (?cond [condition alt] … [else alt])]]{
 Equivalent to nested uses of @racket[?if]. If no @racket[else] clause is
 supplied, then @racket[(?@)], i.e. the empty splice, is used instead.}

@defform[(begin body ...)]{
 Overridden version of @|orig:begin|. Supports ellipses after definitions
 (using @racket[define], @racket[define/with-syntax] or
 @racket[define/syntax-parse]). Supports ellipses after expressions, in which
 case the results are grouped into a splicing list, which makes it possible to
 write @racket[(+ (begin x ...))] and obtain the same result as with
 @racket[(+ x ...)].

 @history[
 #:changed "1.2"
 @elem{Added support @racket[define/syntax-parse], fixed documentation which
   incorrectly claimed support for @racket[define-syntax] instead of
   @racket[define/with-syntax]}]}

@defform*[[(let ([var val] …) . body)
           (let name ([var val] …) . body)]]{
 Overridden version of @|orig:let|. Supports ellipses in the @racket[body]
 after definitions (using @racket[define] and @racket[define-syntax]). Supports
 ellipses after expressions both in the @racket[body] and in the @racket[val].
 In both cases, the results are grouped into a splicing list, which makes it
 possible to write @racket[(let ([vs x ...]) (+ vs))] and obtain the same
 result as with @racket[(+ x ...)].}

@defform[(#%intdef-begin . body)]{
 Equivalent to @racket[begin] from @racketmodname[subtemplate], but assumes
 that it appears directly within the body of a @racket[let] or similar form.

 Third-party macros can cooperate with @racketmodname[subtemplate], allowing
 its features to be used where a sequence of statements is expected. To achieve
 that, the macro would need to detect with @racket[identifier-binding] and
 @racket[syntax-local-introduce] whether @racket[#%intdef-begin] is bound at the
 macro's use-site. If this is the case, then the macro could use
 @racket[#%intdef-begin] instead of @racket[begin].}

@defform*[[(#%app f arg ... . rest)
           (#%app val ooo ...+ expression ...+ . rest)]]{
 Overridden version of @|orig:#%app|, which supports ellipses in the argument
 list. When one of the arguments contains a splicing list, the list's values
 are spliced into the argument list.

 If the first argument is an ellipsis, the @racket[list] function is
 implicitly used, and the first element following @racket[#%app] is interpreted
 as an argument under ellipses.

 A variable appearing in tail position after a dot is appended to the argument
 list, and splicing-lists within are handled.}

@defform[(#%top . var)]{Overridden version of @|orig:#%top|, which is used to
 automatically derive temporary identifiers in expressions. When an unbound
 variable @racket[yᵢ] is used and a matching pattern variable @racket[xᵢ] with
 the same subscript is bound. Note that if a variable @racket[yᵢ] is already
 bound to some value, no attempt will be made to derive temporary identifiers
 for that variable. In contrast, if the identifier @racket[yᵢ] appears, quoted
 by a @racket[subtemplate], then subtemplate will attempt to derive it even if
 it is bound (unless it is bound as a pattern variable).}

@defidform[…]{Alias for @racket[...]}
@defidform[…+]{Alias for @racket[...+]}
 

@section{Overriding the default @racket[#'…] and @racket[#`…]}

@defmodule[subtemplate/override]{
 The @racketmodname[subtemplate/override] module provides the same bindings as
 @racketmodname[subtemplate], but also re-provides @racket[subtemplate] as
 @racket[syntax], and @racket[quasisubtemplate] as @racket[quasisyntax]. This
 allows @racketmodname[subtemplate] to be used via the reader shorthands
 @racket[#'…] and @racket[#`…].}

@section{Limitations}

The derived subscripted identifiers have to be syntactically present within
the template. In particular, if a template metafunction generates a part of a
template containing @racket[yᵢ], it will work only if @racket[yᵢ] is also
present in the "main" part of the template (possibly as an argument to the
template metafunction, or elsewhere).

Currently, template metafunctions defined with
@racketmodname[stxparse-info/parse/experimental/template] are not compatible
with those from @racketmodname[syntax/parse/experimental/template], and vice
versa. There is a pending pull request to make some level of compatibility
possible, so this problem should hopefully be fixed sometime soon.

More generally, there might still be some incompatibilities between
@racketmodname[stxparse-info/parse] and @racketmodname[syntax/parse] (aside
from the fact that @racket[subtemplate] cannot derive @racket[yᵢ] from
@racket[xᵢ] if @racket[xᵢ] was defined by the ``official''
@racketmodname[syntax/parse]), please report them to
@url{https://github.com/jsmaniac/subtemplate/issues}.

The code generated by @racket[subtemplate] is not optimised, so compile-time
and run-time performance will not be as good as with @racket[syntax] or
@racket[template].

The expression splicing-lists are not recognised by templates, and it is not
possible for a template to produce a ``splicing syntax object'' (instead, an
error is raised if a @racket[?@] causes a template to return more than one
syntax object).

Despite the rather extensive test suite, there are likely still some bugs
lurking, please report them to
@url{https://github.com/jsmaniac/subtemplate/issues}.

@subsection{Omitted elements in attributes (via @racket[~optional])}

When some values are missing in the ellipses of a template variable, e.g. via
@racket[~optional], @racket[subtemplate] combines all the existing bound
variables it can find with the correct subscript, in order to fill in as many
elements of the derived variable. For example, if
@racket[(attribute xᵢ)]@note{For readability reasons, we note @racket['(x y)]
 instead of @racket[(list #'x #'y)] here.} returns
@racket['((a #f #f) #f (g h i) #f)], and @racket[(attribute yᵢ)] returns
@racket['(#f (4 5 6) (7 8 9) #f)], then for a derived @racket[zᵢ … …],
@racket[(attribute zᵢ)] will contain
@racket['((a/z xᵢ79/z xᵢ80/z) (4/z 5/z 6/z) (g/z h/z i/z) #f)]. The last
element is @racket[#f], as @racket[subtemplate] lacks enough information to
determine how many elements should be present within the list. The
fully-nested @racket[#f] in @racket['(a #f #f)] are derived, as it is clear at
that point that there is place for only a single omitted element.

If new pattern variables with the same subscript are introduced after a
generated variable was used, they should have the same structure (i.e. missing
sublists in the same positions). Otherwise, the derived variable generated by
@racket[subtemplate] would not contain the same elements before and after that
new pattern variable was introduced.

@include-section{light.scrbl}
