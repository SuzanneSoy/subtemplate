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

(module* test racket/base
  (require (for-syntax racket/pretty)
           (submod ".."))
  (eval #'(begin
            (define-syntax (dbg _stx)
              (parameterize ([pretty-print-columns 188])
                (pretty-print (syntax-local-value #'g1)))
              #'(void))
            (dbg))))