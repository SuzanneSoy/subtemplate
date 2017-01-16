#lang type-expander/lang

#|
Adding fields to the prefix path makes it weaker
Adding fields to the postfix path makes it stronger

(Expand prefix postfix)
=> (and prefixᵢ postfix) …
Also could be expanded as:
=> (and prefix postfixᵢ) …

Rewording ((u pre_x pre_x2) pre_a _ post_b (u post_c post_c2)
=> property holds iff
       pre1 = a
   and (pre2 = x or pre2 = x2)
   and post1 = b
   and (post2 = c or post2 = c2)
|#

(define-type (F A) (I (I A)))
(define-type (I A) (→ A Void))

(define-type eqA1 (F (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1) (G 'u2)))
                             (List (G 'b) (G 'c)))))

(define-type eqB1 (F (∩ (Pairof (List* (G 'a1) (∩ (G 'u1) (G 'u2)))
                                (List (G 'b) (G 'c)))
                        (Pairof (List* (G 'a2) (∩ (G 'u1) (G 'u2)))
                                (List (G 'b) (G 'c))))))

(define-type eqC1 (F (∩ (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1)))
                                (List (G 'b) (G 'c)))
                        (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u2)))
                                (List (G 'b) (G 'c))))))

(define-type weakerD1 (F (∩ (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1)))
                                    (List (G 'b) (G 'c))))))

(define-type strongerE1 (F (∩ (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1) (G 'u2)))
                                      (∩ (List (G 'b) (G 'c))
                                         (List (G 'b2) (G 'c)))))))

(define-type strongerF1 (F (∩ (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1) (G 'u2)))
                                      (Pairof (G 'b) (∩ (List (G 'c))
                                                        (List (G 'c2))))))))

(define-type altF1 (F (∩ (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1) (G 'u2)))
                                 (Pairof (G 'b) (List (G 'c))))
                         (Pairof (List* (∩ (G 'a1) (G 'a2)) (∩ (G 'u1) (G 'u2)))
                                 (Pairof (G 'b) (List (G 'c2)))))))

(ann (ann (λ (x) (void)) eqA1) eqB1)
(ann (ann (λ (x) (void)) eqA1) eqC1)
(ann (ann (λ (x) (void)) eqB1) eqA1)
(ann (ann (λ (x) (void)) eqB1) eqC1)
(ann (ann (λ (x) (void)) eqC1) eqA1)
(ann (ann (λ (x) (void)) eqC1) eqB1)
(ann (ann (λ (x) (void)) eqA1) weakerD1)
(ann (ann (λ (x) (void)) eqB1) weakerD1)
(ann (ann (λ (x) (void)) eqC1) weakerD1)
;(ann (ann (λ (x) (void)) eqA1) strongerD1) ;; rejected, as it should
(ann (ann (λ (x) (void)) strongerE1) eqA1)
;(ann (ann (λ (x) (void)) eqA1) strongerE1) ;; rejected, as it should
(ann (ann (λ (x) (void)) strongerF1) eqA1)
;(ann (ann (λ (x) (void)) eqA1) strongerF1) ;; rejected, as it should
(ann (ann (λ (x) (void)) altF1) eqA1)
;(ann (ann (λ (x) (void)) eqA1) altF1) ;; rejected, as it should
(ann (ann (λ (x) (void)) altF1) strongerF1)
(ann (ann (λ (x) (void)) strongerF1) altF1)




(let ()
  (define-type eqA2 (F (case→ (→ (List 'b 'c) 'a1)
                              (→ (List 'b 'c) 'a2))))

  (define-type eqB2 (F (case→ (→ (List 'b 'c)
                                 (U 'a1 'a2)))))

  (ann (ann (λ (x) (void)) eqA2) eqB2)
  #;(ann (ann (λ (x) (void)) eqB2) eqA2))

;(let ()
(define-type (G A) (F A))
(define-type-expander (+ stx) (syntax-case stx () [(_ . rest) #'(∩ . rest)]))
(define-type-expander (* stx) (syntax-case stx () [(_ . rest) #'(U . rest)]))

(define-type eqA2 (F (+ (* (G 'b) (G 'c) (G 'a1))
                        (* (G 'b) (G 'c) (G 'a2)))))

(define-type eqB2 (F (+ (* (G 'b) (G 'c) (+ (G 'a1) (G 'a2))))))

(define-type Weaker2 (F (+ (* (G 'b) (G 'c) (G 'a1)))))

(ann (ann (λ (x) (void)) eqA2) eqB2)
(ann (ann (λ (x) (void)) eqB2) eqA2)
(ann (ann (λ (x) (void)) eqA2) Weaker2)
(ann (ann (λ (x) (void)) eqB2) Weaker2)
;(ann (ann (λ (x) (void)) Weaker2) eqA2)
;(ann (ann (λ (x) (void)) Weaker2) eqB2)
;)



(let ()
  (define-type weaker3
    (F (∩ (G (Rec R (List* 'a Any R)))
          (G (Rec R (List* Any 'b R))))))
  (define-type stronger3
    (F (∩ (G (List* 'a Any (Rec R (List* 'a Any R))))
          (G (List* Any 'b (Rec R (List* Any 'b R)))))))

  (ann (ann (λ (x) (void)) stronger3) weaker3)
  )

#|
Put the U ∩ inside the positional list?
What about loops of different sizes => won't work
What about merging all the invariants blindly => won't work, but we can
special-case merging these regexp-like invariants, as long as the merging
doesn't need any info about the regexp itself
(e.g. all are "merge the second elements")
|#