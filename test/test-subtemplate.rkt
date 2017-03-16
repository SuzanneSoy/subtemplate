#lang racket
(require subtemplate/private/template-subscripts
         stxparse-info/parse
         stxparse-info/parse/experimental/template
         stxparse-info/case
         phc-toolkit/untyped
         rackunit
         syntax/macro-testing)

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

|#

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate foo)]))
              'foo)

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate foo)]))
              'foo)

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate xⱼ)]))
              'b)

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate xⱼ)]))
              'b)

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate (zᵢ …))]))
              '(c d))

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate (zᵢ …))]))
              '(c d))

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate (wᵢ …))]))
              '(c/w d/w))

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate (wᵢ …))]))
              '(c/w d/w))

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate (kⱼ wᵢ …))]))
              '(b/k c/w d/w))

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate (kⱼ wᵢ …))]))
              '(b/k c/w d/w))

(check-equal? (syntax->datum (syntax-parse #'(a b c d)
                               [(_ xⱼ zᵢ …)
                                (subtemplate (xⱼ kⱼ (zᵢ wᵢ) …))]))
              '(b b/k (c c/w) (d d/w)))

(check-equal? (syntax->datum (syntax-case #'(a b c d) ()
                               [(_ xⱼ zᵢ …)
                                (subtemplate (xⱼ kⱼ (wᵢ zᵢ) …))]))
              '(b b/k (c/w c) (d/w d)))

;; With yᵢ appearing twice:
(check-equal? (syntax->datum (syntax-case #'(a b c) ()
                               [(xᵢ …)
                                (subtemplate (yᵢ … yᵢ …))]))
              '(a/y b/y c/y a/y b/y c/y))




(let ()
  (syntax-parse (syntax-parse #'(a b c d)
                  [(_ xⱼ zᵢ …)
                   (list (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))
                         (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …)))])
    [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
      ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
     (check bound-identifier=? #'x1 #'x2)]))

(syntax-parse (syntax-parse #'(a b c d)
                [(_ xⱼ zᵢ …)
                 (list (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))
                       (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …)))])
  [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'x2)
   (check bound-identifier=? #'w1 #'w2)
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)

   (check bound-identifier=? #'x1 #'b)
   (check bound-identifier=? #'z1 #'c)
   (check bound-identifier=? #'zz1 #'d)
   
   (check bound-identifier=? #'x2 #'b)
   (check bound-identifier=? #'z2 #'c)
   (check bound-identifier=? #'zz2 #'d)

   ;; The *1 are all different:
   (check bound-identifier=? #'x1 #'x1)
   (check (∘ not bound-identifier=?) #'x1 #'w1)
   (check (∘ not bound-identifier=?) #'x1 #'foo1)
   (check (∘ not bound-identifier=?) #'x1 #'z1)
   (check (∘ not bound-identifier=?) #'x1 #'p1)
   (check (∘ not bound-identifier=?) #'x1 #'zz1)
   (check (∘ not bound-identifier=?) #'x1 #'pp1)
   
   (check (∘ not bound-identifier=?) #'w1 #'x1)
   (check bound-identifier=? #'w1 #'w1)
   (check (∘ not bound-identifier=?) #'w1 #'foo1)
   (check (∘ not bound-identifier=?) #'w1 #'z1)
   (check (∘ not bound-identifier=?) #'w1 #'p1)
   (check (∘ not bound-identifier=?) #'w1 #'zz1)
   (check (∘ not bound-identifier=?) #'w1 #'pp1)

   (check (∘ not bound-identifier=?) #'foo1 #'x1)
   (check (∘ not bound-identifier=?) #'foo1 #'w1)
   (check bound-identifier=? #'foo1 #'foo1)
   (check (∘ not bound-identifier=?) #'foo1 #'z1)
   (check (∘ not bound-identifier=?) #'foo1 #'p1)
   (check (∘ not bound-identifier=?) #'foo1 #'zz1)
   (check (∘ not bound-identifier=?) #'foo1 #'pp1)

   (check (∘ not bound-identifier=?) #'z1 #'x1)
   (check (∘ not bound-identifier=?) #'z1 #'w1)
   (check (∘ not bound-identifier=?) #'z1 #'foo1)
   (check bound-identifier=? #'z1 #'z1)
   (check (∘ not bound-identifier=?) #'z1 #'p1)
   (check (∘ not bound-identifier=?) #'z1 #'zz1)
   (check (∘ not bound-identifier=?) #'z1 #'pp1)

   (check (∘ not bound-identifier=?) #'p1 #'x1)
   (check (∘ not bound-identifier=?) #'p1 #'w1)
   (check (∘ not bound-identifier=?) #'p1 #'foo1)
   (check (∘ not bound-identifier=?) #'p1 #'z1)
   (check bound-identifier=? #'p1 #'p1)
   (check (∘ not bound-identifier=?) #'p1 #'zz1)
   (check (∘ not bound-identifier=?) #'p1 #'pp1)

   (check (∘ not bound-identifier=?) #'zz1 #'x1)
   (check (∘ not bound-identifier=?) #'zz1 #'w1)
   (check (∘ not bound-identifier=?) #'zz1 #'foo1)
   (check (∘ not bound-identifier=?) #'zz1 #'z1)
   (check (∘ not bound-identifier=?) #'zz1 #'p1)
   (check bound-identifier=? #'zz1 #'zz1)
   (check (∘ not bound-identifier=?) #'zz1 #'pp1)

   (check (∘ not bound-identifier=?) #'pp1 #'x1)
   (check (∘ not bound-identifier=?) #'pp1 #'w1)
   (check (∘ not bound-identifier=?) #'pp1 #'foo1)
   (check (∘ not bound-identifier=?) #'pp1 #'z1)
   (check (∘ not bound-identifier=?) #'pp1 #'p1)
   (check (∘ not bound-identifier=?) #'pp1 #'zz1)
   (check bound-identifier=? #'pp1 #'pp1)

   ;; The *2 are all different:
   (check bound-identifier=? #'x2 #'x2)
   (check (∘ not bound-identifier=?) #'x2 #'w2)
   (check (∘ not bound-identifier=?) #'x2 #'foo2)
   (check (∘ not bound-identifier=?) #'x2 #'z2)
   (check (∘ not bound-identifier=?) #'x2 #'p2)
   (check (∘ not bound-identifier=?) #'x2 #'zz2)
   (check (∘ not bound-identifier=?) #'x2 #'pp2)
   
   (check (∘ not bound-identifier=?) #'w2 #'x2)
   (check bound-identifier=? #'w2 #'w2)
   (check (∘ not bound-identifier=?) #'w2 #'foo2)
   (check (∘ not bound-identifier=?) #'w2 #'z2)
   (check (∘ not bound-identifier=?) #'w2 #'p2)
   (check (∘ not bound-identifier=?) #'w2 #'zz2)
   (check (∘ not bound-identifier=?) #'w2 #'pp2)

   (check (∘ not bound-identifier=?) #'foo2 #'x2)
   (check (∘ not bound-identifier=?) #'foo2 #'w2)
   (check bound-identifier=? #'foo2 #'foo2)
   (check (∘ not bound-identifier=?) #'foo2 #'z2)
   (check (∘ not bound-identifier=?) #'foo2 #'p2)
   (check (∘ not bound-identifier=?) #'foo2 #'zz2)
   (check (∘ not bound-identifier=?) #'foo2 #'pp2)

   (check (∘ not bound-identifier=?) #'z2 #'x2)
   (check (∘ not bound-identifier=?) #'z2 #'w2)
   (check (∘ not bound-identifier=?) #'z2 #'foo2)
   (check bound-identifier=? #'z2 #'z2)
   (check (∘ not bound-identifier=?) #'z2 #'p2)
   (check (∘ not bound-identifier=?) #'z2 #'zz2)
   (check (∘ not bound-identifier=?) #'z2 #'pp2)

   (check (∘ not bound-identifier=?) #'p2 #'x2)
   (check (∘ not bound-identifier=?) #'p2 #'w2)
   (check (∘ not bound-identifier=?) #'p2 #'foo2)
   (check (∘ not bound-identifier=?) #'p2 #'z2)
   (check bound-identifier=? #'p2 #'p2)
   (check (∘ not bound-identifier=?) #'p2 #'zz2)
   (check (∘ not bound-identifier=?) #'p2 #'pp2)

   (check (∘ not bound-identifier=?) #'zz2 #'x2)
   (check (∘ not bound-identifier=?) #'zz2 #'w2)
   (check (∘ not bound-identifier=?) #'zz2 #'foo2)
   (check (∘ not bound-identifier=?) #'zz2 #'z2)
   (check (∘ not bound-identifier=?) #'zz2 #'p2)
   (check bound-identifier=? #'zz2 #'zz2)
   (check (∘ not bound-identifier=?) #'zz2 #'pp2)

   (check (∘ not bound-identifier=?) #'pp2 #'x2)
   (check (∘ not bound-identifier=?) #'pp2 #'w2)
   (check (∘ not bound-identifier=?) #'pp2 #'foo2)
   (check (∘ not bound-identifier=?) #'pp2 #'z2)
   (check (∘ not bound-identifier=?) #'pp2 #'p2)
   (check (∘ not bound-identifier=?) #'pp2 #'zz2)
   (check bound-identifier=? #'pp2 #'pp2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (define flob (quasisubtemplate (zᵢ …)))
                 (quasisubtemplate (yᵢ …
                                       #,flob
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                       #,(quasisubtemplate (zᵢ …))
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (define flob (syntax-parse #'d [d (quasisubtemplate (zᵢ …))]))
                 (quasisubtemplate (yᵢ …
                                       #,flob
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                       #,(syntax-parse #'d
                                           [d (quasisubtemplate (zᵢ …))])
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) a3 b3 c3)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                       #,(syntax-parse #'d
                                           [d (quasisubtemplate (zᵢ …))])
                                       #,(syntax-parse #'d
                                           [d (quasisubtemplate (zᵢ …))])
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) (a3 b3 c3) a4 b4 c4)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   
   (check bound-identifier=? #'a3 #'a4)
   (check bound-identifier=? #'b3 #'b4)
   (check bound-identifier=? #'c3 #'c4)
   
   (check bound-identifier=? #'a2 #'a4)
   (check bound-identifier=? #'b2 #'b4)
   (check bound-identifier=? #'c2 #'c4)
   
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)])

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                       #,(syntax-parse #'d
                                           [d (quasisubtemplate (kᵢ …))])
                                       #,(syntax-parse #'d
                                           [d (quasisubtemplate (kᵢ …))])
                                       zᵢ …))])
  [(a1 b1 c1 (a2 b2 c2) (a3 b3 c3) a4 b4 c4)
   (check bound-identifier=? #'a2 #'a3)
   (check bound-identifier=? #'b2 #'b3)
   (check bound-identifier=? #'c2 #'c3)
   
   (check (∘ not bound-identifier=?) #'a1 #'a2)
   (check (∘ not bound-identifier=?) #'b1 #'b2)
   (check (∘ not bound-identifier=?) #'c1 #'c2)

   (check (∘ not bound-identifier=?) #'a2 #'a4)
   (check (∘ not bound-identifier=?) #'b2 #'b4)
   (check (∘ not bound-identifier=?) #'c2 #'c4)

   (check (∘ not bound-identifier=?) #'a3 #'a4)
   (check (∘ not bound-identifier=?) #'b3 #'b4)
   (check (∘ not bound-identifier=?) #'c3 #'c4)])

;; Incompatible ellipsis counts
(begin
  (check-exn #rx"incompatible ellipsis match counts for subscripted variables"
             (λ ()
               (syntax-case #'([a b c] [d]) ()
                 [([xᵢ …] [pᵢ …])
                  (quasisubtemplate ([xᵢ …] [pᵢ …] [zᵢ …]))])))

  (check-equal? (syntax->datum
                 (syntax-case #'([a b c] [d]) ()
                   [([xᵢ …] [pᵢ …])
                    (quasisubtemplate ([xᵢ …] [pᵢ …]))]))
                '([a b c] [d]))

  (require (submod "../private/template-subscripts.rkt" test-private))
  (check-exn #rx"incompatible ellipsis match counts for subscripted variables"
             (λ ()
               (generate-nested-ids 1
                                    #'a
                                    #'b
                                    (λ (x) "fmt")
                                    '((foo bar) (baz))
                                    (list #'x #'y)
                                    #'(whole))))
  (check-equal? (map syntax-e
                     (generate-nested-ids 1
                                          #'a
                                          #'b
                                          (λ (x) "fmt")
                                          '((foo bar) (baz quux))
                                          (list #'x #'y)
                                          #'(whole)))
                '(fmt fmt)))

(syntax-parse (syntax-parse #'(a b c)
                [(xᵢ …)
                 (quasisubtemplate (yᵢ …
                                       #,(syntax-parse #'d
                                           [zᵢ (quasisubtemplate (zᵢ))])
                                       #,(syntax-parse #'d
                                           [zᵢ (quasisubtemplate (zᵢ))])
                                       zᵢ …))])
  [(y yy yyy (d1) (d2) z zz zzz)
   (check bound-identifier=? #'d1 #'d2)
   
   (check (∘ not bound-identifier=?) #'y #'yy)
   (check (∘ not bound-identifier=?) #'y #'yyy)
   (check (∘ not bound-identifier=?) #'y #'d1)
   (check (∘ not bound-identifier=?) #'y #'d2)
   (check (∘ not bound-identifier=?) #'y #'z)
   (check (∘ not bound-identifier=?) #'y #'zz)
   (check (∘ not bound-identifier=?) #'y #'zzz)

   (check (∘ not bound-identifier=?) #'yy #'y)
   (check (∘ not bound-identifier=?) #'yy #'yyy)
   (check (∘ not bound-identifier=?) #'yy #'d1)
   (check (∘ not bound-identifier=?) #'yy #'d2)
   (check (∘ not bound-identifier=?) #'yy #'z)
   (check (∘ not bound-identifier=?) #'yy #'zz)
   (check (∘ not bound-identifier=?) #'yy #'zzz)

   (check (∘ not bound-identifier=?) #'yyy #'y)
   (check (∘ not bound-identifier=?) #'yyy #'yy)
   (check (∘ not bound-identifier=?) #'yyy #'d1)
   (check (∘ not bound-identifier=?) #'yyy #'d2)
   (check (∘ not bound-identifier=?) #'yyy #'z)
   (check (∘ not bound-identifier=?) #'yyy #'zz)
   (check (∘ not bound-identifier=?) #'yyy #'zzz)

   (check (∘ not bound-identifier=?) #'d1 #'y)
   (check (∘ not bound-identifier=?) #'d1 #'yy)
   (check (∘ not bound-identifier=?) #'d1 #'yyy)
   ;(check (∘ not bound-identifier=?) #'d1 #'d2)
   (check (∘ not bound-identifier=?) #'d1 #'z)
   (check (∘ not bound-identifier=?) #'d1 #'zz)
   (check (∘ not bound-identifier=?) #'d1 #'zzz)

   (check (∘ not bound-identifier=?) #'d2 #'y)
   (check (∘ not bound-identifier=?) #'d2 #'yy)
   (check (∘ not bound-identifier=?) #'d2 #'yyy)
   ;(check (∘ not bound-identifier=?) #'d2 #'d1)
   (check (∘ not bound-identifier=?) #'d2 #'z)
   (check (∘ not bound-identifier=?) #'d2 #'zz)
   (check (∘ not bound-identifier=?) #'d2 #'zzz)

   (check (∘ not bound-identifier=?) #'z #'y)
   (check (∘ not bound-identifier=?) #'z #'yy)
   (check (∘ not bound-identifier=?) #'z #'yyy)
   (check (∘ not bound-identifier=?) #'z #'d1)
   (check (∘ not bound-identifier=?) #'z #'d2)
   (check (∘ not bound-identifier=?) #'z #'zz)
   (check (∘ not bound-identifier=?) #'z #'zzz)

   (check (∘ not bound-identifier=?) #'zz #'y)
   (check (∘ not bound-identifier=?) #'zz #'yy)
   (check (∘ not bound-identifier=?) #'zz #'yyy)
   (check (∘ not bound-identifier=?) #'zz #'d1)
   (check (∘ not bound-identifier=?) #'zz #'d2)
   (check (∘ not bound-identifier=?) #'zz #'z)
   (check (∘ not bound-identifier=?) #'zz #'zzz)

   (check (∘ not bound-identifier=?) #'zzz #'y)
   (check (∘ not bound-identifier=?) #'zzz #'yy)
   (check (∘ not bound-identifier=?) #'zzz #'yyy)
   (check (∘ not bound-identifier=?) #'zzz #'d1)
   (check (∘ not bound-identifier=?) #'zzz #'d2)
   (check (∘ not bound-identifier=?) #'zzz #'z)
   (check (∘ not bound-identifier=?) #'zzz #'zz)])

(syntax-parse (syntax-parse #'(a b c d)
                [(_ xⱼ zᵢ …)
                 (list (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))
                       (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …)))])
  [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'b)
   (check bound-identifier=? #'foo1 #'foo)
   (check bound-identifier=? #'z1 #'c)
   (check bound-identifier=? #'zz1 #'d)
   
   (check bound-identifier=? #'x2 #'b)
   (check bound-identifier=? #'foo2 #'foo)
   (check bound-identifier=? #'z2 #'c)
   (check bound-identifier=? #'zz2 #'d)

   (check bound-identifier=? #'x1 #'x2)
   (check bound-identifier=? #'w1 #'w2)
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)
   
   (check (∘ not bound-identifier=?) #'x1 #'w1)
   (check (∘ not bound-identifier=?) #'x1 #'p1)
   (check (∘ not bound-identifier=?) #'x1 #'pp1)
   (check (∘ not bound-identifier=?) #'w1 #'x1)
   (check (∘ not bound-identifier=?) #'w1 #'p1)
   (check (∘ not bound-identifier=?) #'w1 #'pp1)
   (check (∘ not bound-identifier=?) #'p1 #'x1)
   (check (∘ not bound-identifier=?) #'p1 #'w1)
   (check (∘ not bound-identifier=?) #'p1 #'pp1)])

(syntax-parse (syntax-parse #'()
                [()
                 (syntax-parse #'(a b)
                   [(zᵢ …)
                    (list (syntax-parse #'(e)
                            [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))])
                          (syntax-parse #'(e) ;; TODO: same test with f
                            [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))]))])])
  [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'e)
   (check bound-identifier=? #'foo1 #'foo)
   (check bound-identifier=? #'z1 #'a)
   (check bound-identifier=? #'zz1 #'b)
   
   (check bound-identifier=? #'x2 #'e)
   (check bound-identifier=? #'foo2 #'foo)
   (check bound-identifier=? #'z2 #'a)
   (check bound-identifier=? #'zz2 #'b)

   (check bound-identifier=? #'x1 #'x2)
   (check (∘ not bound-identifier=?) #'w1 #'w2) ;; yes above, no here.
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)
   
   (check (∘ not bound-identifier=?) #'x1 #'w1)
   (check (∘ not bound-identifier=?) #'x1 #'p1)
   (check (∘ not bound-identifier=?) #'x1 #'pp1)
   (check (∘ not bound-identifier=?) #'w1 #'x1)
   (check (∘ not bound-identifier=?) #'w1 #'p1)
   (check (∘ not bound-identifier=?) #'w1 #'pp1)
   (check (∘ not bound-identifier=?) #'p1 #'x1)
   (check (∘ not bound-identifier=?) #'p1 #'w1)
   (check (∘ not bound-identifier=?) #'p1 #'pp1)])

(syntax-parse (syntax-parse #'()
                [()
                 (syntax-parse #'(a b)
                   [(zᵢ …)
                    (list (syntax-parse #'(e)
                            [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))])
                          (syntax-parse #'(f) ;; above: was e, not f
                            [(xⱼ) (subtemplate ([xⱼ wⱼ] foo [zᵢ pᵢ] …))]))])])
  [(([x1 w1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'e)
   (check bound-identifier=? #'foo1 #'foo)
   (check bound-identifier=? #'z1 #'a)
   (check bound-identifier=? #'zz1 #'b)
   
   (check bound-identifier=? #'x2 #'f) ;; above: was e, not f
   (check bound-identifier=? #'foo2 #'foo)
   (check bound-identifier=? #'z2 #'a)
   (check bound-identifier=? #'zz2 #'b)

   (check (∘ not bound-identifier=?) #'x1 #'x2) ;; yes above, no here.
   (check (∘ not bound-identifier=?) #'w1 #'w2) ;; yes above above, no here.
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)
   
   (check (∘ not bound-identifier=?) #'x1 #'w1)
   (check (∘ not bound-identifier=?) #'x1 #'p1)
   (check (∘ not bound-identifier=?) #'x1 #'pp1)
   (check (∘ not bound-identifier=?) #'w1 #'x1)
   (check (∘ not bound-identifier=?) #'w1 #'p1)
   (check (∘ not bound-identifier=?) #'w1 #'pp1)
   (check (∘ not bound-identifier=?) #'p1 #'x1)
   (check (∘ not bound-identifier=?) #'p1 #'w1)
   (check (∘ not bound-identifier=?) #'p1 #'pp1)])

(syntax-parse (syntax-parse #'()
                [()
                 (syntax-parse #'(a b)
                   [(zᵢ …)
                    (list (syntax-parse #'(c d)
                            [(xᵢ …)
                             (subtemplate ([xᵢ wᵢ] … foo [zᵢ pᵢ] …))])
                          (syntax-parse #'(cc dd)
                            [(xᵢ …)
                             (subtemplate ([xᵢ wᵢ] … foo [zᵢ pᵢ] …))]))])])
  [(([x1 w1] [xx1 ww1] foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] [xx2 ww2] foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'c)
   (check bound-identifier=? #'xx1 #'d)
   (check bound-identifier=? #'foo1 #'foo)
   (check bound-identifier=? #'z1 #'a)
   (check bound-identifier=? #'zz1 #'b)
   
   (check bound-identifier=? #'x2 #'cc)
   (check bound-identifier=? #'xx2 #'dd)
   (check bound-identifier=? #'foo2 #'foo)
   (check bound-identifier=? #'z2 #'a)
   (check bound-identifier=? #'zz2 #'b)

   (check (∘ not bound-identifier=?) #'x1 #'x2)
   (check (∘ not bound-identifier=?) #'xx1 #'xx2)
   (check bound-identifier=? #'w1 #'w2)
   (check bound-identifier=? #'ww1 #'ww2)
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)
   
   (check (∘ not bound-identifier=?) #'x1 #'xx1)
   (check (∘ not bound-identifier=?) #'x1 #'w1)
   (check (∘ not bound-identifier=?) #'x1 #'p1)
   (check (∘ not bound-identifier=?) #'x1 #'pp1)
   (check (∘ not bound-identifier=?) #'xx1 #'x1)
   (check (∘ not bound-identifier=?) #'xx1 #'w1)
   (check (∘ not bound-identifier=?) #'xx1 #'p1)
   (check (∘ not bound-identifier=?) #'xx1 #'pp1)
   (check (∘ not bound-identifier=?) #'w1 #'xx1)
   (check (∘ not bound-identifier=?) #'w1 #'x1)
   (check (∘ not bound-identifier=?) #'w1 #'p1)
   (check (∘ not bound-identifier=?) #'w1 #'pp1)
   (check (∘ not bound-identifier=?) #'p1 #'xx1)
   (check (∘ not bound-identifier=?) #'p1 #'x1)
   (check (∘ not bound-identifier=?) #'p1 #'w1)
   (check (∘ not bound-identifier=?) #'p1 #'pp1)])

(check-exn #px"incompatible ellipsis match counts for subscripted variables"
           (λ ()
             (syntax-parse #'()
               [()
                (syntax-parse #'(a b)
                  [(zᵢ …)
                   (list (syntax-parse #'(c) ;; one here, two above and below
                           [(xᵢ …)
                            (subtemplate ([xᵢ wᵢ] … foo [zᵢ pᵢ] …))])
                         (syntax-parse #'(cc dd)
                           [(xᵢ …)
                            (subtemplate ([xᵢ wᵢ] … foo [zᵢ pᵢ] …))]))])])))

;; Test for arrows, with two maximal candidates tᵢ and zᵢ :
;; the arrow should be drawn to the ᵢ in wᵢ and pᵢ from the ᵢ in the bindings
;; for both tᵢ and zᵢ. For the uses of xᵢ, tᵢ and zᵢ, there should be only one
;; arrow, drawn from the correponding binding.
(syntax-parse (syntax-parse #'()
                [()
                 (syntax-parse #'([a b] [aa bb])
                   [([tᵢ …] [zᵢ …])
                    (list (syntax-parse #'(c d)
                            [(xᵢ …)
                             (subtemplate ([xᵢ wᵢ] … tᵢ … foo [zᵢ pᵢ] …))])
                          (syntax-parse #'(cc dd)
                            [(xᵢ …)
                             (subtemplate ([xᵢ wᵢ] … tᵢ … foo [zᵢ pᵢ] …))]))])])
  [(([x1 w1] [xx1 ww1] t1 tt1 foo1 [z1 p1] [zz1 pp1])
    ([x2 w2] [xx2 ww2] t2 tt2 foo2 [z2 p2] [zz2 pp2]))
   (check bound-identifier=? #'x1 #'c)
   (check bound-identifier=? #'xx1 #'d)
   (check bound-identifier=? #'x2 #'cc)
   (check bound-identifier=? #'xx2 #'dd)
   
   (check bound-identifier=? #'t1 #'a)
   (check bound-identifier=? #'tt1 #'b)
   (check bound-identifier=? #'t2 #'a)
   (check bound-identifier=? #'tt2 #'b)
   
   (check (∘ not bound-identifier=?) #'x1 #'x2)
   (check bound-identifier=? #'w1 #'w2)
   (check (∘ not bound-identifier=?) #'xx1 #'xx2)
   (check bound-identifier=? #'ww1 #'ww2)
   (check bound-identifier=? #'t1 #'t2)
   (check bound-identifier=? #'tt1 #'tt2)
   (check bound-identifier=? #'foo1 #'foo2)
   (check bound-identifier=? #'z1 #'z2)
   (check bound-identifier=? #'p1 #'p2)
   (check bound-identifier=? #'zz1 #'zz2)
   (check bound-identifier=? #'pp1 #'pp2)])

;; Check that the derived values are NOT cached across runs of the same
;; pattern+template (GH bug #1).
(check-equal? (map (λ (v)
                     (syntax->datum
                      (syntax-parse v
                        [(xᵢ …) (subtemplate (yᵢ …))])))
                   (list #'[] #'[a] #'[a b] #'[c d e f]))
              '([] [a/y] [a/y b/y] [c/y d/y e/y f/y]))

(check-equal? (map (λ (v)
                     (syntax->datum
                      (syntax-parse v
                        [(xᵢ …) (subtemplate (xᵢ … yᵢ …))])))
                   (list #'[] #'[a] #'[a b] #'[c d e f]))
              '([] [a a/y] [a b a/y b/y] [c d e f c/y d/y e/y f/y]))

(check-equal? (map (λ (v)
                     (syntax->datum
                      (syntax-parse v
                        [(xᵢ …) (subtemplate ([xᵢ yᵢ] …))])))
                   (list #'[] #'[a] #'[a b] #'[c d e f]))
              '(()
                ([a a/y])
                ([a a/y] [b b/y])
                ([c c/y] [d d/y] [e e/y] [f f/y])))

;; ~optional
(begin
  ;; whole opt present, yᵢ ... ...
  (check-equal? (syntax->datum
                 (syntax-parse #'([(1 2 3) (a b)])
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? (yᵢ ... ...) empty})]))
                '(1/y 2/y 3/y a/y b/y))    

  ;; whole opt empty, yᵢ ... ...
  (check-equal? (syntax->datum
                 (syntax-parse #'()
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? (yᵢ ... ...) empty})]))
                'empty)

  ;; whole opt present, ([xᵢ yᵢ] ...) ...
  (check-equal? (syntax->datum
                 (syntax-parse #'([(1 2 3) (a b)])
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? (([xᵢ yᵢ] ...) ...) empty})]))
                '(([1 1/y] [2 2/y] [3 3/y]) ([a a/y] [b b/y])))

  ;; whole opt empty, ([xᵢ yᵢ] ...) ...
  (check-equal? (syntax->datum
                 (syntax-parse #'()
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? (([xᵢ yᵢ] ...) ...) empty})]))
                'empty)

  ;; whole opt present, (yᵢ ...) ...
  (check-equal? (syntax->datum
                 (syntax-parse #'([(1 2 3) (a b)])
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? ((yᵢ ...) ...) empty})]))
                '((1/y 2/y 3/y) (a/y b/y)))

  ;; whole opt empty, (yᵢ ...) ...
  (check-equal? (syntax->datum
                 (syntax-parse #'()
                   [({~optional ((xᵢ ...) ...)})
                    (subtemplate {?? (yᵢ ... ...) empty})]))
                'empty)

  ;; level-1 opt, (?@ [xᵢ yᵢ] ...)/empty ...
  (check-equal? (syntax->datum
                 (syntax-parse #'((1 2 3) #:kw (a b) #:kw)
                   [({~and {~or (xᵢ ...) #:kw}} ...)
                    (subtemplate ({?? (?@ [xᵢ yᵢ] ...) empty} ...))]))
                '([1 1/y] [2 2/y] [3 3/y] empty [a a/y] [b b/y] empty))

  ;; level-1 opt, (?@ yᵢ ...)/empty ...
  (check-equal? (syntax->datum
                 (syntax-parse #'((1 2 3) #:kw (a b) #:kw)
                   [({~and {~or (xᵢ ...) #:kw}} ...)
                    (subtemplate ({?? (?@ yᵢ ...) empty} ...))]))
                '(1/y 2/y 3/y empty a/y b/y empty))

  ;; level-1 opt, ([xᵢ yᵢ] ...)/empty ...
  (check-equal? (syntax->datum
                 (syntax-parse #'((1 2 3) #:kw (a b) #:kw)
                   [({~and {~or (xᵢ ...) #:kw}} ...)
                    (subtemplate ({?? ([xᵢ yᵢ] ...) empty} ...))]))
                '(([1 1/y] [2 2/y] [3 3/y]) empty ([a a/y] [b b/y]) empty))

  ;; level-1 opt, (xᵢ ...)/empty ...
  (check-equal? (syntax->datum
                 (syntax-parse #'((1 2 3) #:kw (a b) #:kw)
                   [({~and {~or (xᵢ ...) #:kw}} ...)
                    (quasisubtemplate
                     ({?? (xᵢ ...) empty} ...))]))
                '((1 2 3) empty (a b) empty))

  ;; level-1 opt, (yᵢ ...)/empty ...
  (check-equal? (syntax->datum
                 (syntax-parse #'((1 2 3) #:kw (a b) #:kw)
                   [({~and {~or (xᵢ ...) #:kw}} ...)
                    (subtemplate ({?? (yᵢ ...) empty} ...))]))
                '((1/y 2/y 3/y) empty (a/y b/y) empty))

  ;; level-1 opt + same but with all #f filled in.
  (begin
    ;; level-1 opt + same but with all #f filled in. (wᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (wᵢ ...) empty} ...))]))
                  '((e f g)
                    (h i)
                    (j k)
                    (l m n o)))
  
    ;; level-1 opt + same but with some filled/missing. (wᵢ/empty ...) ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate (({?? wᵢ empty} ...) ...))]))
                  '((e f g)
                    (h i)
                    (j k)
                    (l m n o)))
  
    ;; level-1 opt + same but with all #f filled in. ([wᵢ yᵢ] ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? ([wᵢ yᵢ] ...) empty} ...))]))
                  '(([e 1/y] [f 2/y] [g 3/y])
                    ([h h/y] [i i/y])
                    ([j a/y] [k b/y])
                    ([l l/y] [m m/y] [n n/y] [o o/y])))

    ;; level-1 opt + same but with all #f filled in. (yᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (yᵢ ...) empty} ...))]))
                  '((1/y 2/y 3/y)
                    (h/y i/y)
                    (a/y b/y)
                    (l/y m/y n/y o/y)))

    ;; level-1 opt + same but with all #f filled in. (yᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (?@ yᵢ ...) empty} ...))]))
                  '(1/y 2/y 3/y h/y i/y a/y b/y l/y m/y n/y o/y))

    ;; level-1 opt + same but with all #f filled in. ([wᵢ yᵢ/empty] ...) ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate (([wᵢ (?? yᵢ empty)] ...) ...))]))
                  '(([e 1/y] [f 2/y] [g 3/y])
                    ([h h/y] [i i/y])
                    ([j a/y] [k b/y])
                    ([l l/y] [m m/y] [n n/y] [o o/y])))

    ;; level-1 opt + same but with all #f filled in. (yᵢ/empty ...) ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate (((?? yᵢ empty) ...) ...))]))
                  '((1/y 2/y 3/y)
                    (h/y i/y)
                    (a/y b/y)
                    (l/y m/y n/y o/y)))

    ;; level-1 opt + same but with all #f filled in. yᵢ/empty ... ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) (h i) (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [((({~and {~or wᵢ:id #:k}} ...) ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ((?? yᵢ empty) ... ...))]))
                  '(1/y 2/y 3/y
                        h/y i/y
                        a/y b/y
                        l/y m/y n/y o/y)))

  
  ;; level-1 opt + same but with some level-1 #f filled in and some missing
  (begin
    ;; level-1 opt + same with some lvl1 filled/missing. (wᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [(({~and {~or (wᵢ ...) #:k}} ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (wᵢ ...) empty} ...))]))
                  '((e f g)
                    empty
                    (j k)
                    (l m n o)))

    ;; level-1 opt + same with some lvl1 filled/missing. (wᵢ/empty ...) ...
    ;; Invalid because {?? wᵢ empty} ... means to iterate over the known
    ;; elements of wᵢ, and put "empty" if one is absent. However, the whole
    ;; sublist of wᵢ element is missing, so it does not really have a meaningful
    ;; length for the ...
    (check-exn
     #rx"attribute contains non-syntax value.*#f"
     (λ ()
       (convert-compile-time-error
        (check-equal? (syntax->datum
                       (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                        [(1 2 3) #:kw (a b) #:kw])
                         [(({~and {~or (wᵢ ...) #:k}} ...)
                           ({~and {~or (xᵢ ...) #:kw}} ...))
                          (subtemplate (({?? wᵢ empty} ...) ...))]))
                      '((e f g)
                        empty
                        (j k)
                        (l m n o))))))
  
    ;; level-1 opt + same with some lvl1 filled/missing. ([wᵢ yᵢ] ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [(({~and {~or (wᵢ ...) #:k}} ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? ([wᵢ yᵢ] ...) empty} ...))]))
                  '(([e 1/y] [f 2/y] [g 3/y])
                    empty
                    ([j a/y] [k b/y])
                    ([l l/y] [m m/y] [n n/y] [o o/y])))

    ;; level-1 opt + same with some lvl1 #f filled in. (yᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [(({~and {~or (wᵢ ...) #:k}} ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (yᵢ ...) empty} ...))]))
                  '((1/y 2/y 3/y)
                    empty
                    (a/y b/y)
                    (l/y m/y n/y o/y)))

    ;; level-1 opt + same with some lvl1 #f filled in. (yᵢ ...)/empty ...
    (check-equal? (syntax->datum
                   (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                    [(1 2 3) #:kw (a b) #:kw])
                     [(({~and {~or (wᵢ ...) #:k}} ...)
                       ({~and {~or (xᵢ ...) #:kw}} ...))
                      (subtemplate ({?? (?@ yᵢ ...) empty} ...))]))
                  '(1/y 2/y 3/y
                        empty
                        a/y b/y
                        l/y m/y n/y o/y))

    ;; level-1 opt + same with some lvl1 #f filled in. ([wᵢ yᵢ/empty] ...) ...
    ;; Invalid because {?? wᵢ emptywi} ... means to iterate over the known
    ;; elements of wᵢ, and put "empty" if one is absent. However, the whole
    ;; sublist of wᵢ element is missing, so it does not really have a meaningful
    ;; length for the ...
    (check-exn
     #rx"attribute contains non-syntax value.*#f"
     (λ ()
       (convert-compile-time-error
        (check-equal? (syntax->datum
                       (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                        [(1 2 3) #:kw (a b) #:kw])
                         [(({~and {~or (wᵢ ...) #:k}} ...)
                           ({~and {~or (xᵢ ...) #:kw}} ...))
                          (subtemplate
                           (([(?? wᵢ emptywi) (?? yᵢ empty)] ...) ...))]))
                      '(([e 1/y] [f 2/y] [g 3/y])
                        ([emptywi empty] [emptywi empty])
                        ([j a/y] [k b/y])
                        ([l l/y] [m m/y] [n n/y] [o o/y]))))))

    ;; level-1 opt + same with some lvl1 #f filled in. (yᵢ/empty ...) ...
    ;; Invalid because {?? wᵢ empty} ... means to iterate over the known
    ;; elements of wᵢ, and put "empty" if one is absent. However, the whole
    ;; sublist of wᵢ element is missing, so it does not really have a meaningful
    ;; length for the ...
    (check-exn
     #rx"attribute contains non-syntax value.*#f"
     (λ ()
       (convert-compile-time-error
        (check-equal? (syntax->datum
                       (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                        [(1 2 3) #:kw (a b) #:kw])
                         [(({~and {~or (wᵢ ...) #:k}} ...)
                           ({~and {~or (xᵢ ...) #:kw}} ...))
                          (subtemplate (((?? yᵢ empty) ...) ...))]))
                      '((1/y 2/y 3/y)
                        empty
                        (a/y b/y)
                        (l/y m/y n/y o/y))))))

    ;; level-1 opt + same with some lvl1 #f filled in. yᵢ/empty ... ...
    ;; Invalid because {?? yᵢ empty} ... means to iterate over the known
    ;; elements of wᵢ, and put "empty" if one is absent. However, the whole
    ;; sublist of wᵢ element is missing, so it does not really have a meaningful
    ;; length for the ...
    (check-exn
     #rx"attribute contains non-syntax value.*#f"
     (λ ()
       (convert-compile-time-error
        (check-equal? (syntax->datum
                       (syntax-parse #'([(e f g) #:k (j k) (l m n o)]
                                        [(1 2 3) #:kw (a b) #:kw])
                         [(({~and {~or (wᵢ ...) #:k}} ...)
                           ({~and {~or (xᵢ ...) #:kw}} ...))
                          (subtemplate ((?? yᵢ empty) ... ...))]))
                      '(1/y 2/y 3/y
                            empty
                            a/y b/y
                            l/y m/y n/y o/y))))))
  

  ;; level-1 opt + same with some level-2 #f filled in and some missing
  (begin
    ;; level-1 opt + same with some lvl2 filled/missing. (wᵢ ...)/empty ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate ({?? (wᵢ ...) empty} ...))]))
                 '((e f g)
                   empty
                   (j k)
                   (l m n o)))

    ;; level-1 opt + same with some lvl2 filled/missing. (wᵢ/empty ...) ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate (({?? wᵢ empty} ...) ...))]))
                 '((e f g)
                   (h empty)
                   (j k)
                   (l m n o)))
  
    ;; level-1 opt + same with some lvl2 filled/missing. ([wᵢ yᵢ] ...)/empty ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate ({?? ([wᵢ yᵢ] ...) empty} ...))]))
                 '(([e 1/y] [f 2/y] [g 3/y])
                   empty
                   ([j a/y] [k b/y])
                   ([l l/y] [m m/y] [n n/y] [o o/y])))

    ;; level-1 opt + same but with some #f filled in. (yᵢ ...)/empty ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate ({?? (yᵢ ...) empty} ...))]))
                 `((1/y 2/y 3/y)
                   (h/y ,(? symbol?
                            (app symbol->string (regexp #rx"xᵢ[0-9]+/y"))))
                   (a/y b/y)
                   (l/y m/y n/y o/y)))

    ;; level-1 opt + same but with some #f filled in. (yᵢ ...)/empty ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate ({?? (?@ yᵢ ...) empty} ...))]))
                 `(1/y 2/y 3/y
                       h/y ,(? symbol?
                               (app symbol->string (regexp #rx"xᵢ[0-9]+/y")))
                       a/y b/y
                       l/y m/y n/y o/y))

    ;; level-1 opt + same but with some #f filled in. ([wᵢ yᵢ/empty] ...) ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate
                      (([(?? wᵢ emptywi) (?? yᵢ empty)] ...) ...))]))
                 `(([e 1/y] [f 2/y] [g 3/y])
                   ([h h/y]
                    [emptywi
                     ,(? symbol?
                         (app symbol->string (regexp #rx"xᵢ[0-9]+/y")))])
                   ([j a/y] [k b/y])
                   ([l l/y] [m m/y] [n n/y] [o o/y])))

    ;; level-1 opt + same but with some #f filled in. (yᵢ/empty ...) ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate (((?? yᵢ empty) ...) ...))]))
                 `((1/y 2/y 3/y)
                   (h/y ,(? symbol?
                            (app symbol->string (regexp #rx"xᵢ[0-9]+/y"))))
                   (a/y b/y)
                   (l/y m/y n/y o/y)))

    ;; level-1 opt + same but with some #f filled in. yᵢ/empty ... ...
    (check-match (syntax->datum
                  (syntax-parse #'([(e f g) (h #:k) (j k) (l m n o)]
                                   [(1 2 3) #:kw (a b) #:kw])
                    [((({~and {~or wᵢ:id #:k}} ...) ...)
                      ({~and {~or (xᵢ ...) #:kw}} ...))
                     (subtemplate ((?? yᵢ empty) ... ...))]))
                 `(1/y 2/y 3/y
                       h/y ,(? symbol?
                               (app symbol->string (regexp #rx"xᵢ[0-9]+/y")))
                       a/y b/y
                       l/y m/y n/y o/y))))

;; Incompatible shapes of different derived attributes:
(check-exn
 #rx"some derived variables do not have the same ellipsis shape"
 (λ ()
   (convert-compile-time-error
    (syntax-parse #'([1 2 3] #f)
      [({~and {~or (xᵢ ...) #f}} ...)
       (subtemplate ({?? (yᵢ ...) _} ...)) ;; => ((1/y 2/y 3/y) _)
       (syntax-case #'([a b c] [d e]) ()
         ;; introduces elements [d e] which were unknown when yᵢ was
         ;; generated:
         [((wᵢ ...) ...)
          ;; Would give ((a/z b/z c/z) (d/z e/z)), but this is
          ;; inconsistent with the shape of yᵢ.
          (subtemplate ({?? (zᵢ ...) _} ...))])]))))

;; Incompatible shapes of the same attribute if it were generated at two
;; different points.
(check-exn
 #rx"some derived variables do not have the same ellipsis shape"
 (λ ()
   (syntax-parse #'([1 2 3] #f)
     [({~and {~or (xᵢ ...) #f}} ...)
      (subtemplate ({?? (yᵢ ...) _} ...)) ;; => ((1/y 2/y 3/y) _)
      (syntax-case #'([a b c] [d e]) ()
        ;; introduces elements [d e] which were unknown when yᵢ was
        ;; generated:
        [((wᵢ ...) ...)
         ;; Would give ((a/z b/z c/z) (d/z e/z)), but this is
         ;; inconsistent with the shape of yᵢ.
         (subtemplate ({?? (yᵢ ...) _} ...))])])))