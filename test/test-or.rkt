#lang racket

(require subtemplate/ddd
         subtemplate/unsyntax-preparse
         stxparse-info/case
         stxparse-info/parse
         rackunit)

(define (test-??-all v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean}
       {~optional d:keyword})
      (quasitemplate-ddd (?? a b c d))])))

(check-equal? (test-??-all #'(1 x #f #:kw)) '1)
(check-equal? (test-??-all #'(x #f #:kw)) 'x)
(check-equal? (test-??-all #'(#f #:kw)) '#f)
(check-equal? (test-??-all #'(#:kw)) '#:kw)

(check-equal? (test-??-all #'(1)) '1)
(check-equal? (test-??-all #'(x)) 'x)
(check-equal? (test-??-all #'(#f)) '#f)
(check-equal? (test-??-all #'(#:kw)) '#:kw)

(define (test-?cond v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean}
       {~optional d:keyword})
      (quasitemplate-ddd (?cond [a 10] [b 20] [c 30] [d 40]))])))

(check-equal? (test-?cond #'(1 x #f #:kw)) 10)
(check-equal? (test-?cond #'(x #f #:kw)) 20)
(check-equal? (test-?cond #'(#f #:kw)) 30)
(check-equal? (test-?cond #'(#:kw)) 40)

(check-equal? (test-?cond #'(1)) 10)
(check-equal? (test-?cond #'(x)) 20)
(check-equal? (test-?cond #'(#f)) 30)
(check-equal? (test-?cond #'(#:kw)) 40)
