#lang type-expander/lang
(require (for-syntax racket
                     phc-toolkit))

(struct Or ())
(define-type-expander (Invariants stx)
  (syntax-case stx ()
    [(_ invᵢ …)
     #'(→ (U Or (→ invᵢ Void) …) Void)
     #;#'(→ (→ (∩ invᵢ …) Void) Void)]))



(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ T nb)
     #`(define-type T
         (Invariants
          #,@(map (λ (x) #`(List 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k #,x 'm 'n))
                  (range (syntax-e #'nb)))))]))
(foo T0 600)
(foo T1 550)

(define f0 : T0 (λ (x) (void)))

(define-syntax (repeat stx)
  (syntax-case stx ()
    [(_ n body)
     #`(begin #,@(map (const #'body)
                      (range (syntax-e #'n))))]))
(repeat 100
        (ann f0 T1))