#lang racket
(require subtemplate/private/ddd-forms
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped)

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (?? x 'missing) …])
              '(1 missing 3))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?@ 1 2 3))])
              '(1 2 3))

(check-equal? (syntax-parse #'(1 2 3)
                [(x …)
                 (list (x ...) 4 5)])
              '((1 2 3) 4 5))

(check-equal? (syntax-parse #'(1 2 3)
                [(x …)
                 (list (?@ x ...) 4 5)])
              '(1 2 3 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?@ x) ... 4 5)])
              '(1 #f 3 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list ((?@ x) ...) 4 5)])
              '((1 #f 3) 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?@ 'x 'is x) ... 4 5)])
              '(x is 1 x is #f x is 3 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list ((?@ 'x 'is x) ...) 4 5)])
              '((x is 1 x is #f x is 3) 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?? (?@ 'x 'is x) 'nothing-here) ... 4 5)])
              '(x is 1 nothing-here x is 3 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?? (?@ 'x 'is x) (?@ 'nothing 'here)) ... 4 5)])
              '(x is 1 nothing here x is 3 4 5))

(check-equal? (syntax-parse #'(1 #:kw 3)
                [({~and {~or x:nat #:kw}} …)
                 (list (?? (?@ 'x 'is x) (list 'nothing 'here)) ... 4 5)])
              '(x is 1 (nothing here) x is 3 4 5))
