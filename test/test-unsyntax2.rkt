#lang racket
(require subtemplate/override
         rackunit)

(check-equal? (syntax->datum
               (quasitemplate (a b #,(+ 1 1) c)))
              '(a b 2 c))

(check-equal? (syntax->datum
               (template (a b #,(+ 1 1) c)))
              (let ([u 'unsyntax])
                `(a b (,u (+ 1 1)) c)))

(check-equal? (syntax->datum
               (quasitemplate (a b #,@(list (?@ 1 2) (?@ 3 4)) c)))
              '(a b 1 2 3 4 c))

(check-equal? (syntax->datum
               #`(a b #,@(list (?@ 1 2) (?@ 3 4)) c))
              '(a b 1 2 3 4 c))
