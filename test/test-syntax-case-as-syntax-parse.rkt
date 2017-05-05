#lang racket/base
(require rackunit
         subtemplate/private/syntax-case-as-syntax-parse
         stxparse-info/parse)
(check-equal?
 (syntax-parse #'(1 2 3)
   [{~syntax-case (~var ... ~and)}
    (list (map syntax->datum (attribute ~var))
          (syntax->datum (attribute ~and)))
    ])
 '((1 2) 3))

(check-equal?
 (syntax-parse #'(1 2 3)
   [{~syntax-case (... (_ _ _))}
    ;; underscores are not escaped by (... pat)
    (syntax->datum #'_)])
 '_)