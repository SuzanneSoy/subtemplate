#lang typed/racket

(require phc-adt
         (lib "phc-graph/graph-type.hl.rkt"))
(adt-init)

(provide g1)

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

(require (for-syntax racket/pretty
                     racket/base))
(eval #'(begin
          (define-syntax (dbg _stx)
            (parameterize ([pretty-print-columns 188])
              (pretty-print (syntax-local-value #'g1)))
            #'(void))
          (dbg)))

(require (for-syntax syntax/parse
                     "../graph-info.hl.rkt"))

(define-syntax dbg
  (syntax-parser
    [(_ t)
     #`(define-type t
         #,(node-info-promise-type
            (hash-ref (graph-info-nodes (syntax-local-value #'g1)) 'City)))]))
(dbg t-city)
;(define-type expected (t-city Number String Symbol 'Database 'Index))