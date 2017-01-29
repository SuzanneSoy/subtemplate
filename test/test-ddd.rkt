#lang racket
(require subtemplate/ddd
         stxparse-info/case
         stxparse-info/parse
         (only-in racket/base [... …])
         rackunit
         syntax/macro-testing)

(check-equal? (syntax-case #'(1 2 3) ()
                [(x …)
                 (ddd (+ (syntax-e #'x) 3))])
              '(4 5 6))

(check-equal? (syntax-parse #'(1 2 3)
                [(x …)
                 (ddd (+ (syntax-e #'x) 3))])
              '(4 5 6))

(check-exn
 #rx"no pattern variables with depth > 0 were found in the body"
 (λ ()
   (convert-compile-time-error
    (syntax-parse #'(1 2 3)
      [(x y z)
       (ddd (+ (syntax-e #'x) 3))]))))

(check-equal? (syntax-parse #'(1 2 3 4)
                [(x … y)
                 (ddd (+ (syntax-e #'x) (syntax-e #'y)))])
              '(5 6 7))