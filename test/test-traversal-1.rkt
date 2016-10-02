#lang typed/racket

(require "../traversal.hl.rkt"
         "ck.rkt")

(define-fold f₁ t₁ Null String)
;(define-fold f₂ t₂ (Pairof Null Null) String)
;(define-fold f₃ t₃ String String)
;(define-fold f₄ t₄ (Pairof Null String) String)

(define f₁-string->symbol
  (f₁ string?
      (λ ([x : String] [acc : Integer])
        (values (string->symbol x) acc))))
(check-equal?-values: (f₁-string->symbol '() 0)
                      '() 0)

(check-equal?-values: (f₁-string->symbol '() 0)
                      : (Values Null Integer)
                      '() 0)


