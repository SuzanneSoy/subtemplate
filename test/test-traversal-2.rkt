#lang typed/racket

(require "../traversal.hl.rkt"
         type-expander
         phc-adt
         "ck.rkt"
         "../dispatch-union.rkt") ;; DEBUG
(adt-init)

(define-fold f₁ t₁ (tagged tg [a String] [b Boolean]) String)
(define-fold f₂ t₂ (U (tagged tg [a String] [b Boolean])) String)
(define-fold f₃ t₃ (U (tagged tg [a String] [b Boolean])
                      (tagged tg [a Boolean] [c String]))
  String)
(define-fold f₄ t₄ (U (tagged tg [a String] [b Boolean])
                      String
                      (tagged tg [a Boolean] [c String]))
  String)
(define-fold f₅ t₅ (U (tagged t0)
                      String
                      (tagged tg [a Boolean] [c String]))
  String)
(define-fold f₆ t₆ (U String
                      (tagged tg [a String] [b Boolean]))
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

(check-equal?-values:
 ((f₃ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              (tagged tg [a Boolean] [c Symbol]))
           Integer)
 (tagged tg [a 'abc] [b #f]) 1)

(check-equal?-values:
 ((f₃ string? string->symbol+acc) (tagged tg [a #t] [c "def"]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              (tagged tg [a Boolean] [c Symbol]))
           Integer)
 (tagged tg [a #t] [c 'def]) 1)

(check-equal?-values:
   ((f₄ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
   : (Values (U (tagged tg [a Symbol] [b Boolean])
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   (tagged tg [a 'abc] [b #f]) 1)

(check-equal?-values:
   ((f₄ string? string->symbol+acc) "ghi" 0)
   : (Values (U (tagged tg [a Symbol] [b Boolean])
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   'ghi 1)

(check-equal?-values:
   ((f₄ string? string->symbol+acc) (tagged tg [a #t] [c "def"]) 0)
   : (Values (U (tagged tg [a Symbol] [b Boolean])
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   (tagged tg [a #t] [c 'def]) 1)

(check-equal?-values:
   ((f₅ string? string->symbol+acc) (tagged t0 #:instance) 0)
   : (Values (U (tagged t0)
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   (tagged t0 #:instance) 0)

(check-equal?-values:
   ((f₅ string? string->symbol+acc) "ghi" 0)
   : (Values (U (tagged t0)
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   'ghi 1)

(check-equal?-values:
   ((f₅ string? string->symbol+acc) (tagged tg [a #t] [c "def"]) 0)
   : (Values (U (tagged t0)
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   (tagged tg [a #t] [c 'def]) 1)

(check-equal?-values:
 ((f₆ string? string->symbol+acc) (tagged tg [a "abc"] [b #f]) 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              Symbol)
           Integer)
 (tagged tg [a 'abc] [b #f]) 1)

(check-equal?-values:
 ((f₆ string? string->symbol+acc) "ghi" 0)
 : (Values (U (tagged tg [a Symbol] [b Boolean])
              Symbol)
           Integer)
 'ghi 1)
