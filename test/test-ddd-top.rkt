#lang racket

(require subtemplate/private/top-subscripts
         subtemplate/private/ddd-forms
         (only-in subtemplate/private/ddd ddd)
         (except-in subtemplate/private/override ?? ?@)
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped
         (only-in racket/base [... …]))

(check-equal? (syntax-parse #'(a b c)
                [(xᵢ …)
                 yᵢ])
              '(a/y b/y c/y))

(check-equal? (syntax-case #'(a b c) ()
                [(xᵢ …)
                 (yᵢ …)])
              '(a/y b/y c/y))

(check-equal? (syntax-case #'(a b c) ()
                [(xᵢ …)
                 ([list xᵢ yᵢ] …)])
              '([a a/y] [b b/y] [c c/y]))

(check-equal? (syntax-case #'(a b c) ()
                [(xᵢ …)
                 ({?@ xᵢ yᵢ} …)])
              '(a a/y b b/y c c/y))

(check-match (syntax-case #'(a b c) ()
               [(xᵢ …)
                (list #'yᵢ …)])
             (list (? syntax?) (? syntax?) (? syntax?)))

(check-equal? (map syntax->datum
                   (syntax-case #'(a b c) ()
                     [(xᵢ …)
                      (list #'yᵢ …)]))
              '(a/y b/y c/y))

(check-match (syntax-case #'([a b c] [d e]) ()
               [((xᵢ …) …)
                (list (list #'yᵢ …) …)])
             (list (list (? syntax?) (? syntax?) (? syntax?))
                   (list (? syntax?) (? syntax?))))

(check-equal? (map (curry map syntax->datum)
                   (syntax-case #'([a b c] [d e]) ()
                     [((xᵢ …) …)
                      (list (list #'yᵢ …) …)]))
              '([a/y b/y c/y] [d/y e/y]))

(check-match (syntax-case #'([(a1 a2) (b1) (c1 c2 c3)]
                             [(d1 d2 d3 d4) (e1 e2 e3 e4 e5)]) ()
               [(((xᵢ …) …) …)
                (list (list (list #'yᵢ …) …) …)])
             (list (list (list (? syntax?) (? syntax?))
                         (list (? syntax?))
                         (list (? syntax?) (? syntax?) (? syntax?)))
                   (list (list (? syntax?) (? syntax?) (? syntax?) (? syntax?))
                         (list (? syntax?) (? syntax?) (? syntax?)
                               (? syntax?) (? syntax?)))))

(check-equal? (map (curry map (curry map syntax->datum))
                   (syntax-case #'([(a1 a2) (b1) (c1 c2 c3)]
                                   [(d1 d2 d3 d4) (e1 e2 e3 e4 e5)]) ()
                     [(((xᵢ …) …) …)
                      (list (list (list #'yᵢ …) …) …)]))
              '([(a1/y a2/y) (b1/y) (c1/y c2/y c3/y)]
                [(d1/y d2/y d3/y d4/y) (e1/y e2/y e3/y e4/y e5/y)]))

;; CHeck that the same ids are produced.
(check-true (let ([ids (flatten
                        (syntax-case #'(id) ()
                          [(_aᵢ …)
                           (list
                            (ddd #'bᵢ)
                            (list #'bᵢ …)
                            (syntax->list #'(bᵢ …)))]))])
              (andmap (curry apply free-identifier=?)
                      (cartesian-product ids ids))))

(check-true (let ([ids (flatten
                        (syntax-case #'((id)) ()
                          [((aᵢ …) …)
                           (list
                            (ddd (ddd #'bᵢ))
                            (list (list #'bᵢ …) …)
                            (stx-map syntax->list #'((bᵢ …) …))
                            (syntax->list #'(bᵢ … …))
                            (map syntax->list (list #'(bᵢ …) …)))]))])
              (andmap (curry apply free-identifier=?)
                      (cartesian-product ids ids))))

(check-equal? (map (curry map syntax->datum)
                   (syntax-case #'([a b c] [d e]) ()
                     [((xᵢ …) …)
                      (list (list #'yᵢ …) …)]))
              '([a/y b/y c/y] [d/y e/y]))

(check-equal? ((λ (result) (syntax->datum (datum->syntax #f result)))
               (syntax-parse #'[(([h] [i]  10)   ([j] 12  13  [m]))
                                (([a] #:kw #:kw) ([d] [e] [f] [g]))]
                 [[(({~and {~or (yᵢ:id …) :nat}} …) …)
                   (({~and {~or (xᵢ:id …) #:kw}} …) …)]
                  (list (list (?? (list #'zᵢ …) 'missing) …) …)]))
              '(([a/z] [i/z] missing) ([d/z] [e/z] [f/z] [g/z])))

(check-match (syntax-case #'(a b c) ()
               [(xᵢ …)
                ([list xᵢ #'yᵢ] …)])
             (list (list 'a (? syntax?))
                   (list 'b (? syntax?))
                   (list 'c (? syntax?))))

(check-match (syntax-case #'(a b c) ()
               [(xᵢ …)
                ([list #'xᵢ #'yᵢ] …)])
             (list (list (? syntax?) (? syntax?))
                   (list (? syntax?)(? syntax?))
                   (list (? syntax?)(? syntax?))))

(check-match (syntax-case #'(a b c) ()
               [(xᵢ …)
                ({?@ #'xᵢ #'yᵢ} …)])
             (list (? syntax?) (? syntax?)
                   (? syntax?) (? syntax?)
                   (? syntax?) (? syntax?)))

(check-equal? (syntax->datum
               (datum->syntax #f
                              (syntax-case #'(a b c) ()
                                [(xᵢ …)
                                 ([list xᵢ #'yᵢ] …)])))
              '([a a/y] [b b/y] [c c/y]))

(check-equal? (syntax->datum
               (datum->syntax #f
                              (syntax-case #'(a b c) ()
                                [(xᵢ …)
                                 ([list #'xᵢ #'yᵢ] …)])))
              '([a a/y] [b b/y] [c c/y]))

(check-equal? (syntax->datum
               (datum->syntax #f
                              (syntax-case #'(a b c) ()
                                [(xᵢ …)
                                 ({?@ #'xᵢ #'yᵢ} …)])))
              '(a a/y b b/y c c/y))
