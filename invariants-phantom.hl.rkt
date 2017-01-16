#lang aful/unhygienic hyper-literate type-expander/lang

@require[scribble-math
         scribble-enhanced/doc
         "notations.rkt"]

@title[#:style (with-html5 manual-doc-style)
       #:tag "inv-phantom"
       #:tag-prefix "phc-graph/inv-phantom"]{Tracking checked contracts
 via refinement types}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/inv-phantom"))

@section{Introduction}

The cautious compiler writer will no doubt want to check that the graph used
to represent the program verifies some structural properties. For example, the
compiled language might not allow cycles between types. Another desirable
property is that the @racket[in-method] field of an instruction points back to
the method containing it. We will use this second property as a running
example in this section.

@section{Implementation overview : subtyping, variance and phantom types}

It is possible to express with Typed/Racket that a @racket[Method] should
contain a list of @racket[Instruction]s, and that @racket[Instruction]s should
point to a @racket[Method]@note{We are not concerned here about the ability to
 create such values, which necessarily contain some form of cycle. The goal of
 the graph library is indeed to handle the creation and traversal of such
 cyclic data structures in a safe way}:

@chunk[<invariant-1>
       (struct Instruction ([opcode : Byte]
                            [in-method : Method]))
       (struct Method ([body : (Listof Instruction)]))]

This type does not, however, encode the fact that an instruction should point
to the method containing it. Typed/Racket does not really have a notion of
singleton types, aside from symbols and other primitive data. It also lacks a
way to type "the value itself" (e.g. to describe a single-field structure
pointing to itself, possibly via a @racket[Promise]). This means that the
property could only be expressed in a rather contrived way, if it is at all
possible.

@; (define-type Self-Loop (∀ (A) (→ (Pairof Integer (Self-Loop A)))))

We decide to rely instead on a run-time check, i.e. a sort of contract which
checks the structural invariant on the whole graph. In order to let the
type-checker know whether a value was checked against that contract or not, we
include within the node a phantom type which is used as a flag, indicating
that the graph was checked against that contract. This phantom type in a sense
refines the node type, indicating an additional property (which, in our case,
is not checked at compile-time but instead enforced at run-time).

@chunk[<invariant-2>
       (struct (Flag) Instruction ([opcode : Byte]
                                   [in-method : (Method Flag)]))
       (struct (Flag) Method ([body : (Listof (Instruction Flag))]))]

We would then write a function accepting a @racket[Method] for which the
contract @racket[method→instruction→same-method] was checked like this:

@chunk[<invariant-2-use>
       (λ ([m : (Method 'method→instruction→same-method)])
         …)]

Unfortunately, this attempt fails to catch errors as one would expect, because
Typed/Racket discards unused polymorphic arguments, as can be seen in the
following example, which type-checks without any complaint:

@chunk[<phantom-types-ignored>
       (struct (Phantom) S ([x : Integer]))
       (define inst-sa : (S 'a) (S 1))
       (ann inst-sa (S 'b))]

We must therefore make a field with the @racket[Flag] type actually appear
within the instance:

@chunk[<invariant-3>
       (struct (Flag) Instruction ([opcode : Byte]
                                   [in-method : (Method Flag)]
                                   [flag : Flag]))
       (struct (Flag) Method ([body : (Listof (Instruction Flag))]
                              [flag : Flag]))]

Another issue is that the flag can easily be forged. We would therefore like
to wrap it in a struct type which is only accessible by the graph library:

@chunk[<invariant-4>
       (struct (Flag) Flag-Wrapper-Struct ([flag : Flag]))
       (define-type Flag-Wrapper Flag-Wrapper-Struct)
       (code:comment "provide only the type, not the constructor or accessor")
       (provide Flag-Wrapper)]

We would like to be able to indicate that a graph node has validated several
invariants. For that, we need a way to represent the type of a "set" of
invariant witnesses. We also want some subtyping relationship between the
sets: a set @${s₁} with more invariant witnesses should be a subtype of a
subset @${s₂ ⊆ s₁}. We can order the invariant witnesses and use @racket[Rec]
to build the type of a list of invariant witnesses, where some may be missing:

@chunk[<invariant-set-as-List+Rec>
       (define-type At-Least-InvB+InvD
         (Rec R₁ (U (Pairof Any R₁)
                    (Pairof 'InvB (Rec R₂ (U (Pairof Any R₂)
                                             (Pairof 'InvD (Listof Any))))))))]

@chunk[<invariant-set-as-List+Rec-use>
       (ann '(InvA InvB InvC InvD InvE) At-Least-InvB+InvD)
       (ann '(InvB InvD) At-Least-InvB+InvD)
       (code:comment "Rejected, because it lacks 'InvD")
       (code:comment "(ann '(InvB InvC InvE) At-Least-InvB+InvD)")
       (code:comment "The elements must be in the right order,")
       (code:comment "this would be rejected by the typechecker:")
       (code:comment "(ann '(InvD InvB) At-Least-InvB+InvD)")]

Another solution is to group the witnesses in an untagged union with
@racket[U], and place it in a contravariant position:

@chunk[<invariant-set-as-contravariant-U>
       (define-type At-Least-InvB+InvD
         (→ (U 'InvB 'InvD) Void))]

In the case where no invariant is present in the untagged union, the type
@racket[(U)] a.k.a @racket[Nothing], the bottom type with no value, would
appear. This type is somewhat pathological and allows absurd reasoning (a
function accepting @racket[Nothing] can never be called, which may incite the
type checker to perform excessive elision). To avoid any pitfalls, we will
systematically include a dummy element @racket[Or] in the union, to make sure
the union never becomes empty.

This solution also has the advantage that the size of the run-time witness is
constant, and does not depend on the number of checked contracts (unlike the
representation using a list). In practice the function should never be called.
It can however simply be implemented in a way which pleases the type checked
as a function accepting anything and returning void.

In addition to testifying that a graph node was checked against multiple,
separate contracts, there might be some contracts which check stronger
properties than others. A way to encode this relationship in the type system
is to have subtyping relationships between the contract witnesses, so that
@; TODO: get rid of the textit
@${\textit{P}₁(x) ⇒ \textit{P}₂(x) ⇒ \textit{Inv}₁ @texsubtype \textit{Inv}₂}:

@chunk[<invariant-contract-subtyping>
       (struct InvWeak ())
       (struct InvStrong InvWeak ())]

If the witnesses must appear in a contravariant position (when using
@racket[U] to group them), the relationship must be reversed:

@chunk[<invariant-contract-subtyping>
       (struct InvStrongContra ())
       (struct InvWeakContra InvStrongContra ())]

Alternatively, it is possible to use a second contravariant position to
reverse the subtyping relationship again:

@chunk[<invariant-contract-subtyping>
       (struct InvWeak ())
       (struct InvStrong InvWeak ())

       (define InvWeakContra (→ InvWeak Void))
       (define InvStrongContra (→ InvStrong Void))]

Finally, we note that the invariants should always be represented using a
particular struct type, instead of using a symbol, so that name clashes are
not a problem.

@section{Encoding property implication as subtyping}

The witness for a strong property should be a subtype of the witness for a
weaker property. This allows a node with a strong property to be passed where
a node with a weaker property is passed.

@chunk[<structural-draft>
       (code:comment "Draft ideas")
       
       (struct ∈ ())
       (struct ≡ ())
       (struct ∉ ())

       ;(List Rel From Path1 Path2)
       (List ≡ ANodeName (List f1 f2) (List))
       (List ∈ ANodeName (List f1 f2) (List))
       (List ∉ ANodeName (List f1 f2) (List))
       (List ∉ ANodeName (List (* f1 f2 f3 f4) (* f5 f6)) (List))

       ;(List From Path+Rel)
       (List ANodeName (List f1 f2 ≡))
       (List ANodeName (List f1 f2 ∈))
       (List ANodeName (List f1 f2 ∉))
       (List ANodeName (List (List f1 ∉)
                             (List f2 ∉)
                             (List f3 ∉)
                             (List f4
                                   (List f5 ∉)
                                   (List f6 ∉))))
       
       ;; How to make it have the right kind of subtyping?

       
       ]

@subsection{Properties applying to all reachable nodes from @racket[x]}

The property @racket[x ≢ x.**] can be expanded to a series of properties. For
example, if @racket[x] has two fields @racket[a] and @racket[d], the former
itself having two fields @racket[b] and @racket[c], and the latter having a
field @racket[e], itself with a field @racket[f]:
@chunk[<expanded-path-set>
       (x ≢ x.a)
       (x ≢ x.a.b)
       (x ≢ x.a.c)
       (x ≢ x.d)
       (x ≢ x.d.e)
       (x ≢ x.d.e.f)]

@subsection{Prefix trees to the rescue}

This expanded representation is however costly, and can be expressed more
concisely by factoring out the prefixes.

@chunk[<prefixes>
       (x ≢ (x (a (b) (c))
               (d (e (f)))))]

One thing which notably cannot be represented concisely in this way is
@racket[x.a.** ≢ x.b.**], meaning that the subgraphs rooted at @racket[x.a]
and @racket[x.b] are disjoint. It would be possible to have a representation
combining a prefix-tree on the left, and a prefix-tree on the right, implying
the cartesian product of both sets of paths. This has a negligible cost in the
size of the type for the case where one of the members of the cartesian
product, as we end up with the following (the left-hand-side @racket[x] gains
an extra pair of parentheses, because it is now an empty tree):

@chunk[<prefixes>
       ((x) ≢ (x (a (b) (c))
                 (d (e (f)))))]

This does not allow concise expression of all properties, i.e. this is a form
of compression, which encodes concisely likely sets of pairs of paths, but is
of little help for more random properties. For example, if a random subset of
the cartesian product of reachable paths is selected, there is no obvious way
to encode it in a significantly more concise way than simply listing the pairs
of paths one by one.

@subsection{Cycles in properties}

If a @racket[**] path element (i.e. a set of paths representing any path of
any length) corresponds to a part of the graph which contains a cycle in the
type, it is necessary to make that cycle appear in the expanded form. For
that, we use @racket[Rec]. Supposing that the node @racket[x] has two fields,
@racket[a] and @racket[c], the first itself having a field @racket[b] of type
@racket[x]. We would expand @racket[x.**] to the following shape:

@racket[(Rec X (≢ (Node0 'x)
                  (Node2 'x
                         (Field1 'a (Field1 'b (Field1 X)))
                         (Field1 'c))))]

If one of the fields refers not to the root, but to 

TODO: distinction between root nodes and fields in the path. Add an @racket[ε]
component at the root of each path?

@subsection{Partial paths}

Partial paths: if a property holds between @racket[x.a] and @racket[x.b], then
it is stronger than a property which holds between @racket[y.fx.a] and
@racket[y.fx.b] (i.e. the common prefix path narrows down the set of pairs of
values which are related by the property).

A possible solution idea: mask the "beginning" of the path with a @racket[∀]
or @racket[Any]. Either use
@racket[(Rec Ign (U (Field1 Any Ign) Actual-tail-of-type))], or reverse the
``list'', so that one writes @racket[(Field1 'b (Field1 'a Any))], i.e. we
have @racket[(Field1 field-name up)] instead of
@racket[(Field1 field-name children)]. The problem with the reversed version
is that two child fields @racket[b] and @racket[c] need to refer to the same
parent @racket[a], which leads to duplication or naming (in the case of
naming, Typed/Racket tends to perform some inlining anyway, except if tricks
are used to force the type to be recursive (in which case the subtyping / type
matching is less good and fails to recognise weaker or equivalent formulations
of the type)). The problem with the @racket[Rec] solution for an ignored head
of any length is that the number of fields is not known in advance (but
hopefully our representation choices to allow weaker properties with missing
fields could make this a non-problem?).

@subsection{Array and list indices}

When a path reaches an array, list, set or another similar collection, the
special path element @racket[*] can be used to indicate ``any element in the
array or list''. Specific indices can be indicated by an integer, or for lists
with @racket[car], @racket[first], @racket[second], @racket[third] and so on.
The special path elements @racket[cdr] and @racket[rest] access the rest of
the list, i.e. everything but the first element.

@subsection{Other richer properties}

Other richer properties can be expressed, like
@racket[x.len = (length x.somelist)]. This property calls some racket
primitives (@racket[length]), and compares numeric values. However, we do not
attempt to make the type checker automatically recognise weaker or equivalent
properties. Instead, we simply add to the phantom type a literal description
of the checked property, which will only match the same exact property.

@section{Implementation}

@subsection{The witness value}

Since all witnesses will have a type of the form
@racket[(→ (U (→ invᵢ Void) …) Void)], they can all be represented at run-time
by a single value: a function accepting any argument and returning
@racket[Void]. Note that the type of the witness is normally a phantom type,
and an actual value is supplied only because Typed/Racket drops phantom types
before typechecking, as mentioned earlier.

@chunk[<witness-value>
       (: witness-value (→ Any Void))
       (define witness-value (λ (x) (void)))]

@subsection{Grouping multiple invariants}

As mentioned earlier, we group invariants together using an untagged union
@racket[U], which must appear in a contravariant position. We wish to express
witnesses for stronger invariants as subtypes of witnesses for weaker
invariants, and therefore use a second nested function type to flip again the
variance direction. We always include the Or element in the union, to avoid
having an empty union.

@chunk[<grouping-invariants>
       (struct Or ())
       (define-type-expander (Invariants stx)
         (syntax-case stx ()
           [(_ invᵢ …)
            #'(→ (U Or (→ invᵢ Void) …) Void)
            #;#'(→ (→ (∩ invᵢ …) Void) Void)]))]

@subsection{Structural (in)equality and (non-)membership invariants}

@subsubsection{Comparison operator tokens}

We define some tokens which will be used to identify the operator which
relates two nodes in the graph.

@chunk[<comparison-operators>
       (struct (A) inv≡ ([a : A]))
       (struct (A) inv≢ ([a : A]))
       ;(struct inv∈ ()) ;; Can be expressed in terms of ≡
       ;(struct inv∉ ()) ;; Can be expressed in terms of ≢
       ]

@CHUNK[<≡>
       (define-for-syntax (relation inv)
         (syntax-parser
           [(_ (pre-a … {~literal _} post-a …)
               (pre-b … {~literal _} post-b …))
            #:with (r-pre-a …) (reverse (syntax->list #'(pre-a …)))
            #:with (r-pre-b …) (reverse (syntax->list #'(pre-b …)))
            ;; Use U to make it order-independent
            #`(#,inv (U (Pairof (Cycle r-pre-a …)
                                (Cycle post-a …))
                        (Pairof (Cycle r-pre-b …)
                                (Cycle post-b …))))]))

       (define-type-expander ≡ (relation #'inv≡))
       (define-type-expander ≢ (relation #'inv≢))]

@chunk[<cycles>
       (struct ε () #:transparent)
       (struct (T) Target () #:transparent)
       (struct (T) NonTarget Target ([x : T]) #:transparent)
       
       (define-type-expander Cycle
         (syntax-parser
           [(_ field:id … {~literal ↙} loop1:id … (target:id) loop2:id …)
            #'(List* (NonTarget ε)
                     (NonTarget 'field) …
                     (Rec R (List* (NonTarget 'loop1) … ;(NonTarget 'loop1) …
                                   (Target 'target) ;(NonTarget 'target)
                                   (NonTarget 'loop2) … ;(NonTarget 'loop2) …
                                   R)))]
           [(_ field … target)
            ;; TODO: something special at the end?
            #'(List (NonTarget ε) (NonTarget 'field) … (Target 'target))]
           [(_)
            #'(List (Target ε))]))]

@;{@[
   
 ;.a.b = .x.y
 ;(l1=a ∧ l2=b ∧ r1=x ∧ r2=y) ⇒ eq
 ;¬(l1=a ∧ l2=b ∧ r1=x ∧ r2=y) ∨ eq
 ;¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=y ∨ eq

 ;.a.c = .x.y
 ;(l1=a ∧ l2=c ∧ r1=x ∧ r2=y) ⇒ eq

 ;.a.c = .x.z
 ;(l1=a ∧ l2=b ∧ r1=x ∧ r2=z) ⇒ eq
 ;¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=z ∨ eq


 ;.a.b = .x.y ∧ .a.c = .x.z
 ;(¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=y ∨ eq) ∧ (¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=z ∨ eq)
 ;¬¬(¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=y ∨ eq) ∧ (¬l1=a ∨ ¬l2=b ∨ ¬r1=x ∨ ¬r2=z ∨ eq)
 ;¬(l1=a ∧ l2=b ∧ r1=x ∧ r2=y ∧ eq) ∨ (l1=a ∧ l2=b ∧ r1=x ∧ r2=z ∧ ¬eq)
 ]}

@; Problem with ∩: it factors out too much, (∩ '(a . b) '(a . c) '(x . b))
@; becomes (Pairof (∩ 'a 'x) (∩ 'b 'c)), which is equivalent to have all four
@; elements of {a,x} × {b,c}, but we only want three out of these four.

Two sorts of paths inside (in)equality constraints:

@itemlist[
 @item{those anchored on a node, stating that
  @$${
   ∀\ \textit{node} : \textit{NodeType},\quad
   \textit{node}.\textit{path}₁ ≡ \textit{node}.\textit{path}₂}}
 @item{those not anchored on a node, stating that
  @$${
   \begin{array}{c}
   ∀\ \textit{node}₁ : \textit{NodeType}₁,\quad
   ∀\ \textit{node}₂ : \textit{NodeType}₂,\\
   \textit{node}₁.\textit{path}₁ ≡ \textit{node}₂.\textit{path}₂
   \end{array}}}]

@subsection{Putting it all together}

@chunk[<check-a-stronger-b>
       (define-syntax (check-a-stronger-or-same-b stx)
         (syntax-case stx ()
           [(_ stronger weaker)
            (syntax/top-loc stx
              (check-ann (ann witness-value stronger)
                         weaker))]))
       
       (define-syntax (check-a-same-b stx)
         (syntax-case stx ()
           [(_ a b)
            (syntax/top-loc stx
              (begin
                (check-ann (ann witness-value a) b)
                (check-ann (ann witness-value b) a)))]))]

@chunk[<*>
       (require (for-syntax phc-toolkit/untyped
                            syntax/parse))
       
       <witness-value>
       <grouping-invariants>
       <cycles>
       <comparison-operators>
       <≡>

       (module+ test
         (require phc-toolkit)
         <check-a-stronger-b>

         (ann witness-value (Invariants)) ;; No invariants
         (ann witness-value (Invariants (≡ (_ a) (_ a b c))))

         (check-a-stronger-or-same-b (Invariants (≡ (_ a) (_ a b c)))
                                     (Invariants))

         (check-a-same-b (Invariants (≡ (_ a) (_ a b c)))
                         (Invariants (≡ (_ a b c) (_ a))))

         (check-a-stronger-or-same-b (Invariants (≡ (_) (_ b c))
                                                 (≡ (_) (_ b d)))
                                     (Invariants (≡ (_) (_ b c))))
         (check-a-stronger-or-same-b (Invariants (≡ (_) (_ b d))
                                                 (≡ (_) (_ b c)))
                                     (Invariants (≡ (_) (_ b c))))

         (check-a-stronger-or-same-b (Invariants (≡ (_)
                                                    (_ b d a b d ↙ a b (d))))
                                     (Invariants (≡ (_)
                                                    (_ b d ↙ a b (d))))))]
