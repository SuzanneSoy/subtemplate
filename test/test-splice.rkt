#lang racket

(require subtemplate/private/ddd-forms
         rackunit)

(check-equal? (let ([l '(4 5 6)])
                (vector (?@ 1 2 3 . l)))
              #(1 2 3 4 5 6))

(check-equal? (let ([l '(4 5 6)])
                (vector (?@ 1 2 3 (?@ . l) 7 8 9)))
              #(1 2 3 4 5 6 7 8 9))