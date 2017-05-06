#lang racket/base
(require subtemplate/override
         rackunit)
(check-equal? (let ()
                (define/syntax-parse ({~optional
                                       {~or k:keyword b:boolean i:nat}}
                                      {~and {~or (v …) s:str}} …)
                  #'(#:a-keyword (1 2 3 4) "foo" (5 6)))
                #'(l (?@@ (?? (v …)) …)))
              '(l 1 2 3 4 5 6))


(check-equal? (let ()
                (define/syntax-parse ({~optional
                                       {~or k:keyword b:boolean i:nat}}
                                      {~and {~or (v …) s:str}} …)
                  #'(#:a-keyword (1 2 3 4) "foo" (5 6)))
                #'(l (?@@ (?? (v …)) …)))
              '(l 1 2 3 4 5 6))

