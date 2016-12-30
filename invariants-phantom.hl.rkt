#lang aful/unhygienic hyper-literate type-expander/lang

@title[#;#:style #;(with-html5 manual-doc-style)
       #:tag "invariants-phantom"
       #:tag-prefix "phc-graph/invariants-phantom"]{Tracking checked contracts
 via phantom types}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/invariants-phantom"))

@section{Overview}

The cautious compiler writer will no doubt want to check that the graph used
to represent the program verifies some structural properties. For example, the
compiled language might not allow cycles between types. Another desirable
property is that the @racket[in-method] field of an instruction points back to
the method containing it. We will use this second property as a running
example in this section.

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

@; size → use a dummy function which errors when called
@; (accepts an opaque token to prevent calling)

@; Subtyping: multiple contracts with case→

@; Subtyping: when C1 ⇒ C2,
@; make the marker for C1 a subtype of the marker for C2

@chunk[<*>
       (void)]