#lang typed/racket

(require "../traversal.hl.rkt"
         type-expander
         phc-adt
         "ck.rkt")
(adt-init)

(define-type Foo (Listof String))

(define-fold f₁ t₁ (tagged tg [a String] [b Boolean]) String)
(define-fold f₂ t₂ (U (tagged tg [a String] [b Boolean])) String)
(define-fold f₃ t₃ (U (tagged tg [a String] [b Boolean])
                      (tagged tg [a Boolean] [c String]))
  String)


(define (string->symbol+acc [x : String] [acc : Integer])
  (values (string->symbol x) (add1 acc)))

(check-equal?-values:
 ((f₁ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (tagged tg [a Symbol] [b Boolean]) Integer)
 (tagged tg [a 'abc] [b #f]) 1)

(check-equal?-values:
 ((f₂ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])) Integer)
 (tagged tg [a 'abc] [b #f]) 1)

#;(check-equal?-values:
 ((f₃ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              (tagged tg [a Boolean] [c Symbol]))
           Integer)
 (tagged tg [a 'abc] [b #f]) 1)

#;(check-equal?-values:
 ((f₃ string? string->symbol+acc) (tagged tg [a #t] [c "def"]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              (tagged tg [a Boolean] [c Symbol]))
           Integer)
 (tagged tg [a #t] [c 'def]) 1)