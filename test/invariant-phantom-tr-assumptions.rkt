#lang typed/racket

(struct A ())
(struct B A ())
(struct C A ())

(: f (→ (U 'x A) Void))
(define (f _) (void))

(let ()
  (ann f (→ (U B C) Void))
  (ann f (→ (U 'x B C) Void))
  (ann f (→ (U 'x C) Void))
  (ann f (→ (U 'x A C) Void))
  (ann f (→ (U 'x) Void))
  (ann f (→ (U) Void))
  (void))

;;;;;;;;;;

;; Reverse order (BB, CC and DD are more precise invariants than AA)
(struct AA ())
(struct BB AA ())
(struct CC AA ())
(struct DD AA ())

(define-type (Invariant X) (→ X Void))

(: g (→ (U (Invariant 'x) (Invariant BB) (Invariant CC)) Void))
(define (g _) (void))

;; Everything works as expected
(let ()
  (ann g (→ (U (Invariant BB) (Invariant CC)) Void))
  (ann g (→ (U (Invariant 'x) (Invariant BB) (Invariant CC)) Void))
  (ann g (→ (U (Invariant 'x) (Invariant CC)) Void))
  (ann g (→ (U (Invariant 'x)) Void))
  (ann g (→ (U) Void))
  ;; AA works, as it should
  (ann g (→ (U (Invariant 'x) (Invariant AA) (Invariant CC)) Void))
  (ann g (→ (U (Invariant 'x) (Invariant AA)) Void))
  (void))