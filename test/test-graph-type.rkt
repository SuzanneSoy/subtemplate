#lang typed/racket

(require phc-adt
         (lib "phc-graph/graph-type.hl.rkt"))
(adt-init)

(define-graph-type g1
  [City [name : String]
        [streets : (Listof Street)]
        [citizens : (Listof Person)]]
  [Street [name : String]
          [houses : (Listof House)]]
  [House [owner : Person]]
  [Person [name : String]]
  #:invariant City.citizens._ ∈ City.streets._.houses._.owner
  #:invariant City.citizens._ ∋ City.streets._.houses._.owner)