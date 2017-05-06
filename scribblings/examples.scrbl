#lang scribble/manual

@(require racket/require
          scribble/example
          "orig.rkt"
          (for-label subtemplate
                     (only-in syntax/parse/experimental/template)
                     (subtract-in racket/base subtemplate)))

@title{Examples}

This section contains a few (somewhat artificial) examples on how to use
@racketmodname[subtemplate]. Most of the examples here would be more wisely
written as functions, and @racketmodname[syntax/parse] would otherwise be
sufficient with slightly more verbose code. The tools offered by
@racketmodname[subtemplate] are more useful for complex macros, where
boilerplate should be elided as much as possible to leave the true structure
of the macro visible.

@section{Automatically deriving identifiers using subscripts}

When an identifier @racket[yᵢ] is encountered in a template, it is
automatically derived from the corresponding @racket[xᵢ]. In the following
example, @racket[tempᵢ] is implicitly bound to
@racket[#'(a/temp b/temp c/temp)], without the need to call
@racket[generate-temporaries].

@examples[
 (require subtemplate/override)
 (syntax-parse #'(a b c)
   [(vᵢ …)
    #'([tempᵢ vᵢ] …)])]

It is common in macros to save an expression in a temporary variable to avoid
executing it twice. The following example builds on the previous one to do so,
without the need to call @racket[generate-temporaries]. Note that the
temporary identifiers generated this way are hygienic: there will be no name
clashes with identifiers from the user, nor with identifiers directly created
by your macro.

@examples[
 (require racket/require
          (for-syntax (subtract-in racket/base subtemplate/override)
                      subtemplate/override))
 (define-syntax sum
   (syntax-parser
     [(_ vᵢ …)
      #'(let ([tempᵢ vᵢ] …)
          (unless (integer? tempᵢ)
            (printf "Warning: ~a should be an integer, got ~a.\n" 'vᵢ tempᵢ))
          …
          (+ tempᵢ …))]))
 (sum 1 2 3)
 (sum 1 (begin (displayln "executed once") 2) 3)
 (sum 1 (+ 3 0.14) 3)]

If you run out of unicode subscripts, characters following the last @racket[_]
are treated as the subscript:

@examples[
 (require subtemplate/override)
 (syntax-parse #'(a b c)
   [(v_foo …)
    #'([temp_foo v_foo] …)])]

@section{Automatically extracting plain values from syntax objects}

In most cases, you do not have to call @racket[syntax->datum] anymore, as
@racketmodname[subtemplate] implicitly extracts the value of syntax pattern
variables. Do not rely too much on this feature, though, as future versions
may require explicit escapement with a concise shorthand, like
@racket[,pattern-variable] or @RACKET[#,pattern-variable].

@examples[
 #:escape UNSYNTAX
 (require racket/require
          (for-syntax (subtract-in racket/base subtemplate/override)
                      subtemplate/override))
 (define-syntax nested
   (syntax-parser
     [(_ n v)
      (if (> n 0) (code:comment "No need for syntax-e")
          #`(list (nested #,(sub1 n) v)) (code:comment "No need for syntax-e")
          #'v)]))
 (nested 5 '(a b c))]

The implicit @racket[syntax->datum] also works on pattern variables which have
a non-zero ellipsis depth:

@examples[
 (require subtemplate/override)
 (syntax-parse #'(1 2 3 4 5)
   [(v …)
    (define sum (apply + v))
    (if (> sum 10)
        "foo"
        "bar")])]

@section{Function application enhancements}

Why bother ourselves with @racket[apply]? Let's just write what we want:

@examples[
 (require subtemplate/override)
 (syntax-parse #'(1 2 3 4 5)
   [(v …)
    (if (> (+ v …) 10)
        "foo"
        "bar")])]

Ellipses work as you expect when used in expressions:

@examples[
 (require subtemplate/override)
 (define/syntax-parse ((vᵢⱼ …) …) #'((1 2 3 4) (5 6)))
 (define/with-syntax (xₖ …) #'(a b c))
 (+ vᵢⱼ … …)
 (define average (/ (+ vᵢⱼ … …) (length (list vᵢⱼ … …))))
 average
 (max (min vᵢⱼ …) …)
 (list vᵢⱼ … … xₖ …)
 (list (list (+ vᵢⱼ 1) …) … (symbol->string xₖ) …)
 (list (list vᵢⱼ …) … xₖ …)
 (code:comment "Automatically derived symbols:")
 (list (list yᵢⱼ …) …)
 (list yₖ …)
 (code:comment "Same ids as the yₖ ones above:")
 #'(yₖ …)
 ]

Here is another trick with ellipses: @racket[((vᵢ …) …)] should normally call
@racket[1] with arguments @racket[2 3 4], and @racket[5] with the argument
@racket[6], and then call the result of the first with the result of the
second as an argument. Since in most cases this is not what you want, the
@racket[list] function is implicitly called when the second element of an
application form is an ellipsis (do not abuse it, the semantics are a bit at
odds with the usual ones in Racket and might be surprising for people reading
your code):

@examples[
 (require subtemplate/override)
 (define/syntax-parse ((vᵢⱼ …) …) #'((1 2 3 4) (5 6)))
 ((vᵢⱼ …) …)
 (vᵢⱼ … …)
 (((+ vᵢⱼ 1000) …) …)
 (code:comment "Automatically derived symbols:")
 ((yᵢⱼ …) …)
 (yᵢⱼ … …)]

Ellipses surprisingly also work on @racket[define],
@racket[define/with-syntax] and @racket[define/syntax-parse]:

@examples[
 (require subtemplate/override)
 (define/syntax-parse ((v …) …) #'((1 2 3 4) (5 6)))
 (define/syntax-parse (x …) #'("a" "b" "c"))
 (begin
   (define w (+ v 1)) … …
   (define/syntax-parse y:id (string->symbol x)) …)
 w
 #'(y …)]

Since the trick is pulled off by a custom @racket[begin] form, provided by
@racketmodname[subtemplate], it will not work as expected at the REPL unless
you explicitly wrap the define and ellipses with a @racket[begin] form, as
done above. Within a module, however, this should work fine.

@section{Ellipsis-preserving @racket[unsyntax]}

Racket's @orig:syntax and @orig:template from
@racketmodname[syntax/parse/experimental/template] both forget the current
ellipsis position within an @racket[unsyntax] form. This makes it difficult to
perform simple changes to each element of a pattern variable under ellipses.
@racketmodname[syntax/parse/experimental/template] provides template
metafunctions, but they are unpractical for one-off small-scale alterations.
With @racket[subtemplate], @RACKET[#,e] and @RACKET[#,@e] both preserve the
current ellipsis position, meaning that uses of @racket[syntax],
@racket[quasisyntax], @racket[template] and so on within @racket[e] will use
the currently-focused portion of pattern variables under ellipses.

@examples[
 #:escape UNSYNTAX
 (require subtemplate/override racket/list)
 (define sd syntax->datum)
 (define/syntax-parse ((v …) …) #'((1 2 3 4) (5 6)))
 (sd #`(foo #,(+ v …) …))
 (code:comment "Quote, escape, re-quote, re-escape, re-quote:")
 (sd #`(foo #,(cons (length (syntax->list #'(v …)))
                #`(#,(add1 (syntax-e #'v)) …))
        …))
 (code:comment "Concise version of the above:")
 (sd #`(foo (#,(length (v …)) #,(add1 v) …) …))
 (sd #`(foo #,(length (syntax->list #'(v …))) …))
 (sd #`(foo #,(length (list v …)) …))
 (sd #`(foo (#,(add1 v) …) …))
 (sd #`(foo #,(add1 v) … …))
 (sd #`(foo #,@(range v) … …))]

It is still possible to get the traditional full-escape behaviour with
@RACKET[#,,e] instead of @racket[unsyntax], and @RACKET[#,@,e] or
@RACKET[#,,@e] instead of @racket[unsyntax-splicing]:

@examples[
 #:escape UNSYNTAX
 (require subtemplate/override racket/list syntax/stx)
 (define sd syntax->datum)
 (define/syntax-parse ((x v …) …) #'((10 1 2 3 4) (100 5 6)))
 x
 v
 (sd #`(foo (x #,,#'(x …)) …))
 (sd #`(foo (x #,,(stx-map (λ (x) (add1 (syntax-e x))) #'(x …))) …))
 (sd #`(foo (x #,,(list (list (add1 v) …) …)) …))
 (sd #`(foo (x #,,(((add1 v) …) …)) …))
 (sd #`(foo (x #,,(stx-map (λ (x) (length (syntax->list x)))
                           #'((v …) …))) …))
 (sd #`(foo (x #,,((length (v …)) …)) …))
 (sd #`(foo ((v …) #,,((length (v …)) …)) …))
 (sd #`(foo (x #,,@((length (v …)) …)) …))
 (sd #`(foo (x #,@,(range (length (x …)))) …))
 (sd #`(foo (v … #,,@((range (length (v …))) …)) …))]

@section{Splicing and conditional template elements}

The splicing form @racket[?@] as well as @racket[??] should be familiar to
users of @racketmodname[syntax/parse/experimental/template]. The
@racketmodname[subtemplate] library provides overridden versions which also
work outside of syntax templates, as well as a few extras:

@examples[
 (require subtemplate/override)
 (define/syntax-parse ({~optional {~or k:keyword b:boolean i:nat}}
                       {~and {~or (v …) s:str}} …)
   #'(#:a-keyword (1 2 3 4) "foo" (5 6)))
 (list (?? (+ v …)
           (string-length s)) …)
 (list (?? (?@ v …)
           (string-length s)) …)
 (list 'x (?@@ '(y y y) (?? (?@ (list 'c v …))) …) 'z)
 (list (?if s "string" "list of numbers") …)
 (?cond [k (list (?? (?@ 'there-was-a-keyword v …)) …)]
        [b (list (?? (?@ 'there-was-a-boolean (?? v s) …)) …)]
        [else (list (?? (?@ (?? i) v …)) …)])
 (list (?attr k) (?attr b) (?attr i))
 (?? k b i 'none)]

The @racket[?@@] splicing form performs two levels of unwrapping (it can be
understood as a way to perform @racket[(?@ (append elements …))]). The
@racket[(?if _condition _true _false)] is a generalisation of @racket[??],
which accepts a @racket[_condition] template, and produces the
@racket[_true]-template if there are no missing elements in the
@racket[_condition] (in the sense of @racket[~optional]), and produces
@racket[_false] otherwise. @racket[?cond] is a shorthand for a sequence of
nested @racket[?if] forms, and @racket[(?attr a)] returns a boolean indicating
the presence of the attribute (it is a shorthand for @racket[(?if a #t #f)]).
Finally, @racket[??] itself is not limited to two alternatives. When given a
single alternative, @racket[??] implicitly uses @racket[(?@)], i.e. the empty
splice, as the second alternative (this is the behaviour of the version from
@racketmodname[syntax/parse/experimental/template]). When two or more
alternatives are specified, each one is tried in turn, and the last one is
used as a fallback (i.e. an empty splice is @emph{not} implicitly added as a
last alternative when there are already two or more alternatives).

The @racket[?if] form is useful when one would want to write a @racket[??]
form, where the triggering condition should not appear in the left-hand-side
of @racket[??], for example when changing the generated code based on the
presence of a keyword passed to the macro:

@examples[
 (require racket/require
          (for-syntax (subtract-in racket/base subtemplate/override)
                      subtemplate/override))
 (define-syntax my-sort
   (syntax-parser
     [(_ {~optional {~and reverse-kw #:reverse}} v …)
      #'(sort (list v …) (?if reverse-kw > <))]))
 (my-sort 3 2 1)
 (my-sort #:reverse 3 2 1)]

Note that @racket[?@] and @racket[?@@] work on regular lists (but ellipses do
not), and they can splice multiple arguments into the surrounding function
call. One last application trick is the dotted tail argument, used as a
shorthand for @racket[apply]:

@examples[
 (require subtemplate/override racket/function)
 (define l '((1 2 3) (4 5 6)))
 (vector 'a (?@ l) 'c)
 (+ 0 (?@@ (?@@ l)) 7)
 (vector 'a (?@@ (?@@ l)) 'c)
 (+ 0 (?@@ . l) 7)
 (vector 'a (?@@ . l) 'c)
 (map + . l)]