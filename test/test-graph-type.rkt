#lang racket

(define-syntax (mk stx)
  (syntax-case stx ()
    [(_ x)
     #`(define-syntax x
         #,(make-prefab-struct 's (hash)))]))
(mk x)

#|
(require phc-adt
         (lib "phc-graph/graph-type.hl.rkt"))
(adt-init)

#;(define-graph-type g1
    [City [name : String]
          [streets : (Listof Street)]
          [citizens : (Listof Person)]]
    [Street [name : String]
            [houses : (Listof House)]]
    [House [owner : Person]]
    [Person [name : String]]
    #:invariant City.citizens._ ∈ City.streets._.houses._.owner
    #:invariant City.citizens._ ∋ City.streets._.houses._.owner)
|#

