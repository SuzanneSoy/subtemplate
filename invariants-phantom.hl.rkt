#lang aful/unhygienic hyper-literate type-expander/lang

@require[scribble-math
         scribble-enhanced/doc
         "notations.rkt"]

@title[#:style (with-html5 manual-doc-style)
       #:tag "inv-phantom"
       #:tag-prefix "phc-graph/inv-phantom"]{Tracking checked contracts
 via phantom types}

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

@section{Implementation overview}

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
that the graph was checked against that contract.

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

This solution also has the advantage that the size of the run-time witness is
constant, and does not depend on the number of checked contracts (unlike the
representation using a list). In practice the function should never be called.
It can however simply be implemented in a way which pleases the type checked
as a function accepting anything and returning void.

In addition to testifying that a graph node was checked against multiple,
separate contracts, there might be some contracts which check stronger
properties than others. A way to encode this relationship in the type system
is to have subtyping relationships between the contract witnesses, so that
@; TODO: get rid of the mathit
@${\mathit{P}₁(x) ⇒ \mathit{P}₂(x) ⇒ \mathit{Inv}₁ @texsubtype \mathit{Inv}₂}:

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

@section{Types for some graph contracts}

@chunk[<structural>
       (≡ [a : Nd] (get a f1 f2))
       (∈ [a : Nd] (get a f1 f2))
       (∉ [a : Nd] (get a f1 f2))
       (∉ [a : Nd] (get a (* f1 f2 f3 f4) (* f5 f6)))]

@chunk[<*>
       (void)]