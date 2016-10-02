#lang info
(define collection "phc-graph")
(define deps '("base"
               "rackunit-lib"
               "phc-toolkit"
               "phc-adt"
               "type-expander"
               "hyper-literate"
               "scribble-enhanced"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/phc-graph.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '("Georges Dupéron"))
