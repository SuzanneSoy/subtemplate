#lang typed/racket

(require "../traversal.hl.rkt")

(define-fold f₁ t₁ Null String)
;(define-fold f₂ t₂ (Pairof Null Null) String)
;(define-fold f₃ t₃ String String)
;(define-fold f₄ t₄ (Pairof Null String) String)

((f₁ string?
     (λ ([x : String] [acc : Integer])
       (values (string->symbol x) acc)))
 '()
 0)