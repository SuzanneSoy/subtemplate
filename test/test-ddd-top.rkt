#lang racket

(require subtemplate/top-subscripts
         subtemplate/ddd-forms
         (except-in subtemplate/override ?? ?@)
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped
         (only-in racket/base [... …]))

#;(check-equal? (syntax-parse #'(a b c)
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