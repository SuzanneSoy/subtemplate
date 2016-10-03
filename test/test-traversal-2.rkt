#lang typed/racket

(require "../traversal.hl.rkt"
         type-expander
         phc-adt
         "ck.rkt")
(adt-init)

#;(define-type Foo (Listof String))

(define-fold f₁ t₁ (tagged tg [a String] [b Boolean]) String)

(define (string->symbol+acc [x : String] [acc : Integer])
  (values (string->symbol x) (add1 acc)))

(check-equal?-values:
 ((f₁ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (tagged tg [a Symbol] [b Boolean]) Integer)
 (tagged tg [a 'abc] [b #f]) 1)


