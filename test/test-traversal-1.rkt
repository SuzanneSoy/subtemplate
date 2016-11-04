#lang type-expander

(require "../traversal.hl.rkt"
         "ck.rkt")

(define-type Foo (Listof String))

(define-fold f₁ t₁ Null String)
(define-fold f₂ t₂ (Pairof Null Null) String)
(define-fold f₃ t₃ String String)
(define-fold f₄ t₄ (Pairof Null String) String)
(define-fold f₅ t₅ (Listof Null) String)
(define-fold f₆ t₆ (List Null (Pairof Null Null) Null) String)
(define-fold f₇ t₇ (Listof String) String)
(define-fold f₈ t₈ (List String Foo (Listof String)) String)
(define-fold f₉ t₉ (List (Listof String) Foo (Listof String)) (Listof String))
(define-fold f₁₀ t₁₀ (List String Foo (Listof String)) (Listof String))
(define-fold f₁₁ t₁₁ (List (Listof String) (Listof Number)) (Listof String))
(define-fold f₁₂ t₁₂ (List (Listof String) (Listof String)) (Listof String))

(define (string->symbol+acc [x : String] [acc : Integer])
  (values (string->symbol x) (add1 acc)))

(check-equal?-values: ((f₁ string? string->symbol+acc) '() 0)
                      '() 0)

(check-equal?-values: ((f₁ string? string->symbol+acc) '() 0)
                      : (Values Null Integer)
                      '() 0)

(check-equal?-values: ((f₂ string? string->symbol+acc) '(() . ()) 0)
                      : (Values (Pairof Null Null) Integer)
                      '(() . ()) 0)

(check-equal?-values: ((f₃ string? string->symbol+acc) "abc" 0)
                      : (Values Symbol Integer)
                      'abc 1)

(check-equal?-values: ((f₄ string? string->symbol+acc) '(() . "def") 0)
                      : (Values (Pairof Null Symbol) Integer)
                      '(() . def) 1)

(check-equal?-values: ((f₅ string? string->symbol+acc) '(() () () ()) 0)
                      : (Values (Listof Null) Integer)
                      '(() () () ()) 0)

(check-equal?-values: ((f₅ string? string->symbol+acc) '(()) 0)
                      : (Values (Listof Null) Integer)
                      '(()) 0)

(check-equal?-values: ((f₅ string? string->symbol+acc) '() 0)
                      : (Values (Listof Null) Integer)
                      '() 0)

(check-equal?-values: ((f₆ string? string->symbol+acc) '(() (() . ()) ()) 0)
                      : (Values (List Null (Pairof Null Null) Null) Integer)
                      '(() (() . ()) ()) 0)

(check-equal?-values: ((f₇ string? string->symbol+acc) '("abc" "def" "ghi") 0)
                      : (Values (Listof Symbol) Integer)
                      '(abc def ghi) 3)

(check-equal?-values: ((f₈ string? string->symbol+acc) '("abc" ("def" "ghi")
                                                               ("jkl" "mno"))
                                                       0)
                      : (Values (List Symbol (Listof String) (Listof Symbol))
                                Integer)
                      '(abc ("def" "ghi") (jkl mno)) 3)

(check-equal?-values: ((f₉ (make-predicate (Listof String))
                           (λ ([l : (Listof String)] [acc : Integer])
                             (values (map string->symbol l)
                                     (add1 acc))))
                       '(("a" "b" "c")
                         ("def" "ghi")
                         ("jkl" "mno"))
                       0)
                      : (Values (List (Listof Symbol)
                                      (Listof String)
                                      (Listof Symbol))
                                Integer)
                      '((a b c) ("def" "ghi") (jkl mno)) 2)

(check-equal?-values: ((f₁₀ (make-predicate (Listof String))
                           (λ ([l : (Listof String)] [acc : Integer])
                             (values (map string->symbol l)
                                     (add1 acc))))
                       '("abc"
                         ("def" "ghi")
                         ("jkl" "mno"))
                       0)
                      : (Values (List String
                                      (Listof String)
                                      (Listof Symbol))
                                Integer)
                      '("abc" ("def" "ghi") (jkl mno)) 1)
