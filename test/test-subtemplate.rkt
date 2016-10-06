#lang racket
(require "../subtemplate.rkt"
         phc-toolkit/untyped
         rackunit)

(map syntax->datum
     (syntax-parse #'(a b c d)
       [(_ xⱼ zᵢ …)
        (list (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))
              (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …)))]))

(map syntax->datum
     (syntax-parse #'()
       [()
        (syntax-parse #'(a b)
          [(zᵢ …)
           (list (syntax-parse #'(e)
                   [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))])
                 (syntax-parse #'(e)
                   [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))]))])]))

#|
(define-syntax (tst stx)
  (syntax-case stx ()
    [(_ tt)
     #`'#,(find-subscript-binder #'tt #f)]))

(check-false (syntax-case #'(a b) ()
               [(_ x)
                (tst x)]))

(check-equal? (syntax-parse
                  #'(a b c)
                [(_ x yᵢ)
                 (list (tst x)
                       (tst wᵢ))])
              '(#f yᵢ))

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate foo)]))
              'foo)

(syntax-parse (syntax-parse #'(a b c d)
                [(_ xⱼ zᵢ …)
                 (list (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))
                       (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …)))])
  [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
   (check free-identifier=? #'x1 #'x2)
   (check free-identifier=? #'w1 #'w2)
   (check free-identifier=? #'foo1 #'foo2)
   (check free-identifier=? #'z1 #'z2)
   (check free-identifier=? #'p1 #'p2)
   (check free-identifier=? #'zz1 #'zz2)
   (check free-identifier=? #'pp1 #'pp2)

   (check free-identifier=? #'x1 #'b)
   (check free-identifier=? #'z1 #'c)
   (check free-identifier=? #'zz1 #'d)
   
   (check free-identifier=? #'x2 #'b)
   (check free-identifier=? #'z2 #'c)
   (check free-identifier=? #'zz2 #'d)

   ;; The *1 are all different:
   (check free-identifier=? #'x1 #'x1)
   (check (∘ not free-identifier=?) #'x1 #'w1)
   (check (∘ not free-identifier=?) #'x1 #'foo1)
   (check (∘ not free-identifier=?) #'x1 #'z1)
   (check (∘ not free-identifier=?) #'x1 #'p1)
   (check (∘ not free-identifier=?) #'x1 #'zz1)
   (check (∘ not free-identifier=?) #'x1 #'pp1)
   
   (check (∘ not free-identifier=?) #'w1 #'x1)
   (check free-identifier=? #'w1 #'w1)
   (check (∘ not free-identifier=?) #'w1 #'foo1)
   (check (∘ not free-identifier=?) #'w1 #'z1)
   (check (∘ not free-identifier=?) #'w1 #'p1)
   (check (∘ not free-identifier=?) #'w1 #'zz1)
   (check (∘ not free-identifier=?) #'w1 #'pp1)

   (check (∘ not free-identifier=?) #'foo1 #'x1)
   (check (∘ not free-identifier=?) #'foo1 #'w1)
   (check free-identifier=? #'foo1 #'foo1)
   (check (∘ not free-identifier=?) #'foo1 #'z1)
   (check (∘ not free-identifier=?) #'foo1 #'p1)
   (check (∘ not free-identifier=?) #'foo1 #'zz1)
   (check (∘ not free-identifier=?) #'foo1 #'pp1)

   (check (∘ not free-identifier=?) #'z1 #'x1)
   (check (∘ not free-identifier=?) #'z1 #'w1)
   (check (∘ not free-identifier=?) #'z1 #'foo1)
   (check free-identifier=? #'z1 #'z1)
   (check (∘ not free-identifier=?) #'z1 #'p1)
   (check (∘ not free-identifier=?) #'z1 #'zz1)
   (check (∘ not free-identifier=?) #'z1 #'pp1)

   (check (∘ not free-identifier=?) #'p1 #'x1)
   (check (∘ not free-identifier=?) #'p1 #'w1)
   (check (∘ not free-identifier=?) #'p1 #'foo1)
   (check (∘ not free-identifier=?) #'p1 #'z1)
   (check free-identifier=? #'p1 #'p1)
   (check (∘ not free-identifier=?) #'p1 #'zz1)
   (check (∘ not free-identifier=?) #'p1 #'pp1)

   (check (∘ not free-identifier=?) #'zz1 #'x1)
   (check (∘ not free-identifier=?) #'zz1 #'w1)
   (check (∘ not free-identifier=?) #'zz1 #'foo1)
   (check (∘ not free-identifier=?) #'zz1 #'z1)
   (check (∘ not free-identifier=?) #'zz1 #'p1)
   (check free-identifier=? #'zz1 #'zz1)
   (check (∘ not free-identifier=?) #'zz1 #'pp1)

   (check (∘ not free-identifier=?) #'pp1 #'x1)
   (check (∘ not free-identifier=?) #'pp1 #'w1)
   (check (∘ not free-identifier=?) #'pp1 #'foo1)
   (check (∘ not free-identifier=?) #'pp1 #'z1)
   (check (∘ not free-identifier=?) #'pp1 #'p1)
   (check (∘ not free-identifier=?) #'pp1 #'zz1)
   (check free-identifier=? #'pp1 #'pp1)

   ;; The *2 are all different:
   (check free-identifier=? #'x2 #'x2)
   (check (∘ not free-identifier=?) #'x2 #'w2)
   (check (∘ not free-identifier=?) #'x2 #'foo2)
   (check (∘ not free-identifier=?) #'x2 #'z2)
   (check (∘ not free-identifier=?) #'x2 #'p2)
   (check (∘ not free-identifier=?) #'x2 #'zz2)
   (check (∘ not free-identifier=?) #'x2 #'pp2)
   
   (check (∘ not free-identifier=?) #'w2 #'x2)
   (check free-identifier=? #'w2 #'w2)
   (check (∘ not free-identifier=?) #'w2 #'foo2)
   (check (∘ not free-identifier=?) #'w2 #'z2)
   (check (∘ not free-identifier=?) #'w2 #'p2)
   (check (∘ not free-identifier=?) #'w2 #'zz2)
   (check (∘ not free-identifier=?) #'w2 #'pp2)

   (check (∘ not free-identifier=?) #'foo2 #'x2)
   (check (∘ not free-identifier=?) #'foo2 #'w2)
   (check free-identifier=? #'foo2 #'foo2)
   (check (∘ not free-identifier=?) #'foo2 #'z2)
   (check (∘ not free-identifier=?) #'foo2 #'p2)
   (check (∘ not free-identifier=?) #'foo2 #'zz2)
   (check (∘ not free-identifier=?) #'foo2 #'pp2)

   (check (∘ not free-identifier=?) #'z2 #'x2)
   (check (∘ not free-identifier=?) #'z2 #'w2)
   (check (∘ not free-identifier=?) #'z2 #'foo2)
   (check free-identifier=? #'z2 #'z2)
   (check (∘ not free-identifier=?) #'z2 #'p2)
   (check (∘ not free-identifier=?) #'z2 #'zz2)
   (check (∘ not free-identifier=?) #'z2 #'pp2)

   (check (∘ not free-identifier=?) #'p2 #'x2)
   (check (∘ not free-identifier=?) #'p2 #'w2)
   (check (∘ not free-identifier=?) #'p2 #'foo2)
   (check (∘ not free-identifier=?) #'p2 #'z2)
   (check free-identifier=? #'p2 #'p2)
   (check (∘ not free-identifier=?) #'p2 #'zz2)
   (check (∘ not free-identifier=?) #'p2 #'pp2)

   (check (∘ not free-identifier=?) #'zz2 #'x2)
   (check (∘ not free-identifier=?) #'zz2 #'w2)
   (check (∘ not free-identifier=?) #'zz2 #'foo2)
   (check (∘ not free-identifier=?) #'zz2 #'z2)
   (check (∘ not free-identifier=?) #'zz2 #'p2)
   (check free-identifier=? #'zz2 #'zz2)
   (check (∘ not free-identifier=?) #'zz2 #'pp2)

   (check (∘ not free-identifier=?) #'pp2 #'x2)
   (check (∘ not free-identifier=?) #'pp2 #'w2)
   (check (∘ not free-identifier=?) #'pp2 #'foo2)
   (check (∘ not free-identifier=?) #'pp2 #'z2)
   (check (∘ not free-identifier=?) #'pp2 #'p2)
   (check (∘ not free-identifier=?) #'pp2 #'zz2)
   (check free-identifier=? #'pp2 #'pp2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (define flob (quasisubtemplate (zᵢ …)))
                 (quasisubtemplate (yᵢ …
                                    #,flob
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,(quasisubtemplate (zᵢ …))
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)])
;; the test above is not exactly right (zᵢ will still have the correct
;; binding), but it gives the general idea.

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (define flob (syntax-parse #'d [d (quasisubtemplate (zᵢ …))]))
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,flob
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,(syntax-parse #'d
                                        [d (quasisubtemplate (zᵢ …))])
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,(syntax-parse #'d
                                        [d (quasisubtemplate (zᵢ …))])
                                    #,(syntax-parse #'d
                                        [d (quasisubtemplate (zᵢ …))])
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) (a3 b3 c3) a4 b4 c4)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   
   (check free-identifier=? #'a3 #'a4)
   (check free-identifier=? #'b3 #'b4)
   (check free-identifier=? #'c3 #'c4)
   
   (check free-identifier=? #'a2 #'a4)
   (check free-identifier=? #'b2 #'b4)
   (check free-identifier=? #'c2 #'c4)
   
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,(syntax-parse #'d
                                        [d (quasisubtemplate (kᵢ …))])
                                    #,(syntax-parse #'d
                                        [d (quasisubtemplate (kᵢ …))])
                                    zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) (a3 b3 c3) a4 b4 c4)
   (check free-identifier=? #'a2 #'a3)
   (check free-identifier=? #'b2 #'b3)
   (check free-identifier=? #'c2 #'c3)
   
   (check (∘ not free-identifier=?) #'a1 #'a2)
   (check (∘ not free-identifier=?) #'b1 #'b2)
   (check (∘ not free-identifier=?) #'c1 #'c2)

   (check (∘ not free-identifier=?) #'a2 #'a4)
   (check (∘ not free-identifier=?) #'b2 #'b4)
   (check (∘ not free-identifier=?) #'c2 #'c4)

   (check (∘ not free-identifier=?) #'a3 #'a4)
   (check (∘ not free-identifier=?) #'b3 #'b4)
   (check (∘ not free-identifier=?) #'c3 #'c4)])
|#

#;(map syntax->datum
       (syntax-parse #'(a b c)
         [(xᵢ …)
          (list (syntax-parse #'(d)
                  [(pᵢ …) #`(#,(quasisubtemplate (xᵢ … pᵢ … zᵢ …))
                             #,(quasisubtemplate (xᵢ … pᵢ … zᵢ …)))])
                (syntax-parse #'(e)
                  [(pᵢ …) (quasisubtemplate (xᵢ … pᵢ … zᵢ …))]))]))

#;(syntax->datum
   (syntax-parse #'(a b c)
     [(xᵢ …)
      (quasisubtemplate (yᵢ …
                         ;; must be from xᵢ, not yᵢ
                         #,(syntax-parse #'(d)
                             [(pᵢ …) (quasisubtemplate (pᵢ … zᵢ …))])
                         ;; GIVES WRONG ID (re-uses the one above, shouldn't):
                         #,(syntax-parse #'(e)
                             [(pᵢ …) (quasisubtemplate (pᵢ … zᵢ …))])
                         wᵢ …))]))

#|
(syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                    ;; must be from xᵢ, not yᵢ
                                    #,(syntax-parse #'d
                                        [zᵢ (quasisubtemplate (zᵢ …))])
                                    #,(syntax-parse #'e
                                        [zᵢ (quasisubtemplate (zᵢ …))])
                                    zᵢ …))])
|#