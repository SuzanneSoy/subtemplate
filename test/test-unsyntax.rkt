#lang racket/base

(require subtemplate/private/top-subscripts
         subtemplate/private/ddd-forms
         subtemplate/private/unsyntax-preparse
         subtemplate/private/template-subscripts
         (except-in subtemplate/private/override ?? ?@)
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped
         (only-in racket/base [... …]))

(check-equal? (syntax->datum
               (syntax-parse #'(1 2 3)
                 [(x …)
                  (quasisubtemplate-ddd (x …))]))
              '(1 2 3))

(check-equal? (syntax->datum
               (syntax-case #'(1 2 3) ()
                 [(x …)
                  (quasisubtemplate-ddd (#,(+ x 4) …))]))
              '(5 6 7))

(check-equal? (syntax->datum
               (syntax-case #'(1 2 3) ()
                 [(x …)
                  (quasisubtemplate-ddd (a b c))]))
              '(a b c))

(check-equal? (syntax->datum
               (syntax-case #'(1 2 3) ()
                 [(xᵢ …)
                  (quasisubtemplate-ddd (#,(cons yᵢ (+ xᵢ 4)) …))]))
              '([1/y . 5] [2/y . 6] [3/y . 7]))

(check-equal? (syntax->datum
               (syntax-case #'(1 2 3) ()
                 [(xᵢ …)
                  (quasisubtemplate-ddd (#,@(list yᵢ (+ xᵢ 4)) …))]))
              '(1/y 5 2/y 6 3/y 7))

(check-equal? (syntax->datum
               (syntax-case #'(1 2 3) ()
                 [(xᵢ …)
                  (quasisubtemplate-ddd (#,(?@ yᵢ (+ xᵢ 4)) …))]))
              '(1/y 5 2/y 6 3/y 7))

(check-equal? (syntax->datum
               (syntax-parse #'([1 2 3] [a #:kw c])
                 [([xᵢ …] [{~and {~or zᵢ:id #:kw}} …])
                  (quasisubtemplate-ddd (#,(?? #'zᵢ (?@ #'yᵢ (+ xᵢ 4))) …))]))
              '(a 2/y 6 c))

(check-equal? (syntax->datum
               (syntax-case #'([1 2 3] [4 5 6]) ()
                 [([x …] …)
                  (quasisubtemplate-ddd ((#,(- x) …) …))]))
              '((-1 -2 -3) (-4 -5 -6)))

(check-equal? (syntax->datum
               (syntax-case #'([1 2 3] [4 5 6]) ()
                 [([x …] …)
                  (quasisubtemplate-ddd (([#,(- x) #,,x] …) …))]))
              (let ([l '((1 2 3) (4 5 6))])
                `(([-1 ,l] [-2 ,l] [-3 ,l]) ([-4 ,l] [-5 ,l] [-6 ,l]))))

(check-equal? (syntax->datum
               (syntax-case #'([1 2 3] [4 5 6]) ()
                 [([x …] …)
                  (quasisubtemplate-ddd (([#,(- x) #,,@x] …) …))]))
              (let ([l '((1 2 3) (4 5 6))])
                `(([-1 ,@l] [-2 ,@l] [-3 ,@l]) ([-4 ,@l] [-5 ,@l] [-6 ,@l]))))