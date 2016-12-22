#lang hyper-literate typed/racket/base #:no-auto-require
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

@chunk[<graph>
       (define-syntax define-graph
         (syntax-parser
           [<signature>
            <implementation>]))]

@chunk[<signature>
       (_ _name
          [[_nodeᵢ [_fieldᵢⱼ :colon _τᵢⱼ] …] …]
          [[(_mappingₖ [_argₖₗ _τₖₗ] …) :colon _return-typeₖ . _bodyₖ] …])]

@chunk[<implementation>
       #'()]

@section{Overview of the implementation (draft)}

@chunk[<implementation-draft>
       <create-Qₖ>
       <re-bind-mappings>
       <define-indices>
       <process-queues>]

@chunk[<define-indices>
       (define/with-syntax (_indexₖ …) (stx-map gensym #'(_idxₖ …)))
       #'(begin
           (define-type _indexₖ (graph-index '_indexₖ))
           …)]

@chunk[<define-index>
       (struct (K) graph-index ([key : K] [index : Index]))]

Create one queue @racket[_Qₖ] for each mapping:

@chunk[<create-Qₖ>
       #'(begin
           (define _Qₖ <create-queue>)
           (define _Qₖ-enqueue <TODO>)
           (define _Qₖ-pop <TODO>)
           …)]

Re-bind mappings to catch outbound calls:

@chunk[<re-bind-mappings>
       #'(let ([_mappingₖ _make-placeholderₖ] …)
           . bodyₖ)]

Define functions which enqueue into a given @racket[_Qₖ] and start processing.
The final @racket[_name] macro dispatches to these functions.

@chunk[<entry-pointₖ>
       #'(begin
           (define (_entry-pointₖ _argₖₗ …)
             (entry-point #:mappingₖ (list (list _argₖₗ …))))
           …)]

These are based upon the main @racket[entry-point], which takes any number of
initial elements to enqueue, and processes the queues till they are all empty.

@chunk[<entry-point>
       #'(define (entry-point #:mappingₖ [_argsₖ* : (Listof (List τₖₗ …)) '()])
           (for ([_argsₖ (in-list _argsₖ*)])
             (let-values ([(_argₖₗ …) _argsₖ])
               (Qₖ-enqueue _argₖₗ …))))]

@chunk[<process-queues>
       (until queues are all empty
              process item, see below)]

@itemlist[
 @item{Find and replace references to old nodes and new incomplete nodes and
  new placeholder nodes, instead insert indices.}
 @item{Problem: we need to actually insert indices for references to nodes,
  not for references to mappings (those have to be inlined).}]


@chunk[<*>
       (require racket/require
                (for-syntax (subtract-in (combine-in racket/base
                                                     syntax/parse)
                                         "subtemplate-override.rkt")
                            phc-toolkit/untyped
                            type-expander/expander
                            "subtemplate-override.rkt")
                "traversal.hl.rkt"
                phc-toolkit)
       <define-index>
       <graph>]