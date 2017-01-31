#lang racket
(require subtemplate/ddd
         stxparse-info/case
         stxparse-info/parse
         (only-in racket/base [... …])
         rackunit
         syntax/macro-testing
         syntax/stx)

(check-equal? (syntax-case #'((1 2 3) (4 5)) ()
                [((x …) …)
                 (ddd (list (length (syntax->list #'(x …)))
                            (ddd (+ (syntax-e #'x) 3))))])
              '([3 (4 5 6)]
                [2 (7 8)]))

(check-equal? (syntax-case #'(1 2 3) ()
                [(x …)
                 (ddd (+ (syntax-e #'x) 3))])
              '(4 5 6))

(check-equal? (syntax-parse #'(1 2 3)
                [(x …)
                 (ddd (+ (syntax-e #'x) 3))])
              '(4 5 6))

(check-equal? (syntax-case #'(((1 2) (3)) ((4 5 6))) ()
                  [(((x …) …) …)
                   (ddd (list (length (syntax->list #'((x …) …)))
                              (length (syntax->list #'(x … …)))
                              (ddd (ddd (- (syntax-e #'x))))))])
                '([2 3 ((-1 -2) (-3))]
                  [1 3 ((-4 -5 -6))]))

(check-equal? (syntax-case #'([1 2 3] [a]) ()
                [([x …] [y …])
                 (ddd (+ (syntax-e #'x) 3))])
              '(4 5 6))

(check-equal? (syntax-case #'(([1 2 3] [a])) ()
                [(([x …] [y …]) …)
                 (ddd (ddd (+ (syntax-e #'x) 3)))])
              '((4 5 6)))

;; The inner ddd should not make the outer one consider the variables actually
;; used. This test will break if y is considered to be used, because it does not
;; have the same shape as x anywhere, so map will complain that the lists do not
;; have the same length.
(check-equal? (syntax-case #'([#:xs (1 2 3) (4 5)]
                              [#:ys (a) (b) (c) (d)]) ()
                [([#:xs (x …) …]
                  [#:ys (y …) …])
                 (ddd (ddd (+ (syntax-e #'x) 3)))])
              '((4 5 6) (7 8)))

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

(check-equal? (syntax-case #'((1 2 3) (4 5)) ()
                [((x …) …)
                 (ddd (list (length (syntax->list #'(x …)))
                            (ddd (+ (syntax-e #'x) 3))))])
              '([3 (4 5 6)]
                [2 (7 8)]))