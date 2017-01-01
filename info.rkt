#lang info
(define collection "phc-graph")
(define deps '("base"
               "rackunit-lib"
               "https://github.com/jsmaniac/phc-toolkit.git#dev"
               "https://github.com/jsmaniac/phc-adt.git?path=phc-adt#dev"
               "https://github.com/jsmaniac/type-expander.git#Let-Λ"
               "hyper-literate"
               "scribble-enhanced"
               "typed-racket-lib"
               "srfi-lite-lib"
               "delay-pure"
               "backport-template-pr1514"
               "typed-map"
               "scribble-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "remember"
                     "typed-racket-doc"
                     "aful"
                     "scribble-math"))
(define scribblings
  '(("scribblings/phc-graph.scrbl" ()
                                   ("Data Structures"))
    ("scribblings/phc-graph-implementation.scrbl" (multi-page)
                                                  ("Data Structures"))))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '("Georges Dupéron"))
