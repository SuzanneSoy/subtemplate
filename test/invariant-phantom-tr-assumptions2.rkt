#lang typed/racket

(: f (→ (→ (U (Rec R (List (List 'a R)
                           (List 'b R)))
              (Rec R (List (List 'a R)
                           (List 'c R))))
           Void)
        Void))
(define (f x) (void))

(ann f (→ (→ (U (Rec K (List (List 'a K) (List 'c K)))
                (Rec W (List (List 'a W) (List 'b W))))
             Void) Void))

(ann f (→ (→ (U (Rec W (List (List 'a W) (List 'b W)))
                (Rec K (List (List 'a K) (List 'c K))))
             Void) Void))

(: g (→ (→ (Rec A (Rec B (List (List 'a A)
                               (List 'b B))))
           Void)
        Void))
(define (g x) (void))

(ann g
     (→ (→ (Rec B (Rec A (List (List 'a A)
                               (List 'b B))))
           Void)
        Void))

(ann g
     (→ (→ (Rec X (List (List 'a X)
                        (List 'b X)))
           Void)
        Void))

(define-type (≡ X Y) (List '≡ X Y))

(: h (→ (→ (∀ (X1 X2) (→ (U (≡ (List 'a 'b X1)
                               (List 'c 'd X1))
                            (≡ (List 'e 'f X2)
                               (List 'g 'g X2)))))
           Void)
        Void))
(define (h x) (void))


(ann (λ ([x : (Rec R (Pairof 'a (Pairof 'b R)))]) (void))
       (-> (Rec R (Pairof 'a (Pairof 'b R))) Void))

(ann (λ ([x : (Rec R (Pairof 'a (Pairof 'b R)))]) (void))
       (-> (Pairof 'a (Rec R (Pairof 'b (Pairof 'a R)))) Void))

(ann (λ ([x : (Rec R (List 'a (List 'b R)))]) (void))
       (-> (List 'a (Rec R (List 'b (List 'a R)))) Void))

(ann (λ ([x : (Rec R (List 'a R (List 'b R)))]) (void))
       (-> (Rec R (Pairof 'a (Pairof R (Pairof (List 'b R) Null)))) Void))

(ann (λ ([x : (Rec R (List 'a R (List 'b R)))]) (void))
       (-> (Pairof 'a (Rec R (Pairof (Pairof 'a R) (Pairof (List 'b (Pairof 'a R)) Null)))) Void))

(ann (λ ([x : (Rec R (List 'a R (List 'b R)))]) (void))
       (-> (Pairof 'a (Pairof (Pairof 'a
                                      (Rec R (Pairof (Pairof 'a R) (Pairof (List 'b (Pairof 'a R)) Null)))
                                      )
                              (Pairof (List 'b (Pairof 'a
                                                       (Rec R (Pairof (Pairof 'a R) (Pairof (List 'b (Pairof 'a R)) Null)))
                                                       )) Null))) Void))