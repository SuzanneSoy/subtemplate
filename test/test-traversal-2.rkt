#lang typed/racket

(require "../traversal.hl.rkt"
         type-expander
         phc-adt
         "ck.rkt"
         "../dispatch-union.rkt") ;; DEBUG
(adt-init)

#;(define-type Foo (Listof String))

(define-fold f₁ t₁ (tagged tg [a String] [b Boolean]) String)
(define-fold f₂ t₂ (U (tagged tg [a String] [b Boolean])) String)
(define-fold f₃ t₃ (U (tagged tg [a String] [b Boolean])
                      (tagged tg [a Boolean] [c String]))
  String)
#;(define-fold f₄ t₄ (U (tagged tg [a String] [b Boolean])
                        String
                        (tagged tg [a Boolean] [c String]))
    String)
#;(define-fold f₄ t₄ (U (tagged t0)
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

#;(check-equal?-values:
   ((f₄ string? string->symbol+acc) (tagged tg [a #t] [c "def"]) 0)
   : (Values (U (tagged tg [a Symbol] [b Boolean])
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   (tagged tg [a #t] [c 'def]) 1)

#;(check-equal?-values:
   ((f₄ string? string->symbol+acc) "ghi" 0)
   : (Values (U (tagged tg [a Symbol] [b Boolean])
                Symbol
                (tagged tg [a Boolean] [c Symbol]))
             Integer)
   'ghi 1)

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










#|



(begin
  #;(define-fold
      _Xⱼ/_fxⱼ-test-traversal-2279088
      _Xⱼ/_txⱼ-test-traversal-2279086
      (tagged tg (a String) (b Boolean))
      String)
  #;(define-fold
      _Xⱼ/_fxⱼ-test-traversal-2279089
      _Xⱼ/_txⱼ-test-traversal-2279087
      String
      String)
  (define-type
    (t₄ type-to-replaceᵢ/_Tᵢ-test-traversal-2279083)
    (U
     (_Xⱼ/_txⱼ-test-traversal-2279086
      type-to-replaceᵢ/_Tᵢ-test-traversal-2279083)
     (_Xⱼ/_txⱼ-test-traversal-2279087
      type-to-replaceᵢ/_Tᵢ-test-traversal-2279083)))
  (:
   f₄
   (∀
    (type-to-replaceᵢ/_Aᵢ-test-traversal-2279094
     type-to-replaceᵢ/_Bᵢ-test-traversal-2279093
     Acc)
    (→
     (→ Any Boolean : type-to-replaceᵢ/_Aᵢ-test-traversal-2279094)
     (→
      type-to-replaceᵢ/_Aᵢ-test-traversal-2279094
      Acc
      (Values type-to-replaceᵢ/_Bᵢ-test-traversal-2279093 Acc))
     (→
      (t₄ type-to-replaceᵢ/_Aᵢ-test-traversal-2279094)
      Acc
      (Values (t₄ type-to-replaceᵢ/_Bᵢ-test-traversal-2279093) Acc)))))
  (define ((f₄
            type-to-replaceᵢ/predicateᵢ-test-traversal-2279082
            type-to-replaceᵢ/updateᵢ-test-traversal-2279081)
           v
           acc)
    (cond
      ((type-to-replaceᵢ/predicateᵢ-test-traversal-2279082 v)
       ((_Xⱼ/_fxⱼ-test-traversal-2279089
         type-to-replaceᵢ/predicateᵢ-test-traversal-2279082
         type-to-replaceᵢ/updateᵢ-test-traversal-2279081)
        v
        acc))
      (((tagged? tg a b) v)
       ((_Xⱼ/_fxⱼ-test-traversal-2279088
         type-to-replaceᵢ/predicateᵢ-test-traversal-2279082
         type-to-replaceᵢ/updateᵢ-test-traversal-2279081)
        v
        acc)))
    #;(dispatch-union
       v
       ((String
         type-to-replaceᵢ/Aᵢ-test-traversal-2279091
         type-to-replaceᵢ/predicateᵢ-test-traversal-2279082))
       ((tagged tg (a String) (b Boolean))
        ((_Xⱼ/_fxⱼ-test-traversal-2279088
          type-to-replaceᵢ/predicateᵢ-test-traversal-2279082
          type-to-replaceᵢ/updateᵢ-test-traversal-2279081)
         v
         acc))
       (String
        ((_Xⱼ/_fxⱼ-test-traversal-2279089
          type-to-replaceᵢ/predicateᵢ-test-traversal-2279082
          type-to-replaceᵢ/updateᵢ-test-traversal-2279081)
         v
         acc)))))
(begin
  #;(define-fold
      _Xⱼ/_fxⱼ-test-traversal-2279102
      _Xⱼ/_txⱼ-test-traversal-2279100
      String
      String)
  #;(define-fold
      _Xⱼ/_fxⱼ-test-traversal-2279103
      _Xⱼ/_txⱼ-test-traversal-2279101
      Boolean
      String)
  (define-type
    (_Xⱼ/_txⱼ-test-traversal-2279086
     type-to-replaceᵢ/_Tᵢ-test-traversal-2279098)
    (tagged
     tg
     (a
      :
      (_Xⱼ/_txⱼ-test-traversal-2279100
       type-to-replaceᵢ/_Tᵢ-test-traversal-2279098))
     (b
      :
      (_Xⱼ/_txⱼ-test-traversal-2279101
       type-to-replaceᵢ/_Tᵢ-test-traversal-2279098))))
  (:
   _Xⱼ/_fxⱼ-test-traversal-2279088
   (∀
    (type-to-replaceᵢ/_Aᵢ-test-traversal-2279108
     type-to-replaceᵢ/_Bᵢ-test-traversal-2279107
     Acc)
    (→
     (→ Any Boolean : type-to-replaceᵢ/_Aᵢ-test-traversal-2279108)
     (→
      type-to-replaceᵢ/_Aᵢ-test-traversal-2279108
      Acc
      (Values type-to-replaceᵢ/_Bᵢ-test-traversal-2279107 Acc))
     (→
      (_Xⱼ/_txⱼ-test-traversal-2279086
       type-to-replaceᵢ/_Aᵢ-test-traversal-2279108)
      Acc
      (Values
       (_Xⱼ/_txⱼ-test-traversal-2279086
        type-to-replaceᵢ/_Bᵢ-test-traversal-2279107)
       Acc)))))
  (define ((_Xⱼ/_fxⱼ-test-traversal-2279088
            type-to-replaceᵢ/predicateᵢ-test-traversal-2279097
            type-to-replaceᵢ/updateᵢ-test-traversal-2279096)
           v
           acc)
    (let*-values (((_Xⱼ/_resultⱼ-test-traversal-2279104 acc)
                   ((_Xⱼ/_fxⱼ-test-traversal-2279102
                     type-to-replaceᵢ/predicateᵢ-test-traversal-2279097
                     type-to-replaceᵢ/updateᵢ-test-traversal-2279096)
                    (uniform-get v a)
                    acc))
                  ((_Xⱼ/_resultⱼ-test-traversal-2279105 acc)
                   ((_Xⱼ/_fxⱼ-test-traversal-2279103
                     type-to-replaceᵢ/predicateᵢ-test-traversal-2279097
                     type-to-replaceᵢ/updateᵢ-test-traversal-2279096)
                    (uniform-get v b)
                    acc)))
      (values
       (tagged
        tg
        #:instance
        (a _Xⱼ/_resultⱼ-test-traversal-2279104)
        (b _Xⱼ/_resultⱼ-test-traversal-2279105))
       acc))))
(begin
  (define-type
    (_Xⱼ/_txⱼ-test-traversal-2279100
     type-to-replaceᵢ/_Tᵢ-test-traversal-2279112)
    type-to-replaceᵢ/_Tᵢ-test-traversal-2279112)
  (:
   _Xⱼ/_fxⱼ-test-traversal-2279102
   (∀
    (type-to-replaceᵢ/_Aᵢ-test-traversal-2279115
     type-to-replaceᵢ/_Bᵢ-test-traversal-2279114
     Acc)
    (→
     (→ Any Boolean : type-to-replaceᵢ/_Aᵢ-test-traversal-2279115)
     (→
      type-to-replaceᵢ/_Aᵢ-test-traversal-2279115
      Acc
      (Values type-to-replaceᵢ/_Bᵢ-test-traversal-2279114 Acc))
     (→
      (_Xⱼ/_txⱼ-test-traversal-2279100
       type-to-replaceᵢ/_Aᵢ-test-traversal-2279115)
      Acc
      (Values
       (_Xⱼ/_txⱼ-test-traversal-2279100
        type-to-replaceᵢ/_Bᵢ-test-traversal-2279114)
       Acc)))))
  (define ((_Xⱼ/_fxⱼ-test-traversal-2279102
            type-to-replaceᵢ/predicateᵢ-test-traversal-2279111
            type-to-replaceᵢ/updateᵢ-test-traversal-2279110)
           v
           acc)
    (type-to-replaceᵢ/updateᵢ-test-traversal-2279110 v acc)))
(begin
  (define-type
    (_Xⱼ/_txⱼ-test-traversal-2279101
     type-to-replaceᵢ/_Tᵢ-test-traversal-2279119)
    Boolean)
  (:
   _Xⱼ/_fxⱼ-test-traversal-2279103
   (∀
    (type-to-replaceᵢ/_Aᵢ-test-traversal-2279122
     type-to-replaceᵢ/_Bᵢ-test-traversal-2279121
     Acc)
    (→
     (→ Any Boolean : type-to-replaceᵢ/_Aᵢ-test-traversal-2279122)
     (→
      type-to-replaceᵢ/_Aᵢ-test-traversal-2279122
      Acc
      (Values type-to-replaceᵢ/_Bᵢ-test-traversal-2279121 Acc))
     (→
      (_Xⱼ/_txⱼ-test-traversal-2279101
       type-to-replaceᵢ/_Aᵢ-test-traversal-2279122)
      Acc
      (Values
       (_Xⱼ/_txⱼ-test-traversal-2279101
        type-to-replaceᵢ/_Bᵢ-test-traversal-2279121)
       Acc)))))
  (define ((_Xⱼ/_fxⱼ-test-traversal-2279103
            type-to-replaceᵢ/predicateᵢ-test-traversal-2279118
            type-to-replaceᵢ/updateᵢ-test-traversal-2279117)
           v
           acc)
    (values v acc)))
(begin
  (define-type
    (_Xⱼ/_txⱼ-test-traversal-2279087
     type-to-replaceᵢ/_Tᵢ-test-traversal-2279128)
    type-to-replaceᵢ/_Tᵢ-test-traversal-2279128)
  (:
   _Xⱼ/_fxⱼ-test-traversal-2279089
   (∀
    (type-to-replaceᵢ/_Aᵢ-test-traversal-2279131
     type-to-replaceᵢ/_Bᵢ-test-traversal-2279130
     Acc)
    (→
     (→ Any Boolean : type-to-replaceᵢ/_Aᵢ-test-traversal-2279131)
     (→
      type-to-replaceᵢ/_Aᵢ-test-traversal-2279131
      Acc
      (Values type-to-replaceᵢ/_Bᵢ-test-traversal-2279130 Acc))
     (→
      (_Xⱼ/_txⱼ-test-traversal-2279087
       type-to-replaceᵢ/_Aᵢ-test-traversal-2279131)
      Acc
      (Values
       (_Xⱼ/_txⱼ-test-traversal-2279087
        type-to-replaceᵢ/_Bᵢ-test-traversal-2279130)
       Acc)))))
  (define ((_Xⱼ/_fxⱼ-test-traversal-2279089
            type-to-replaceᵢ/predicateᵢ-test-traversal-2279127
            type-to-replaceᵢ/updateᵢ-test-traversal-2279126)
           v
           acc)
    (type-to-replaceᵢ/updateᵢ-test-traversal-2279126 v acc)))|#