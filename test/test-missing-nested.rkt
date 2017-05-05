#lang racket
(require subtemplate/override
         rackunit)

(check-equal? (syntax-parse #'((yy 1 2) #:kw (yyy 3 4))
                [({~and {~or (y x …) :keyword}} …)
                 (syntax->datum
                  (quasitemplate
                   (0 #,(list (quasitemplate
                               (?? (~~~
                                    y
                                    #,(list x) ...
                                    ~~~)
                                   oops)))
                      ... 9)))])
              '(0 ((~~~ yy (1) (2) ~~~)) (oops) ((~~~ yyy (3) (4) ~~~)) 9))

(check-equal?
 (syntax-parse #'((y a b 3 d 5 f) #:kw (z g 8 i) #:kww)
   [({~and {~or (y {~and {~or x:id _}} …) :keyword}} …)
    (syntax->datum
     (quasitemplate
      (0 #,(list (quasitemplate
                  (?? (~~~
                       y
                       #,(list (template (?? x -)) (template (?? x -))) ...
                       ~~~)
                      oops)))
         ... 9)))])
 '(0
   ((~~~ y (a a) (b b) (- -) (d d) (- -) (f f) ~~~))
   (oops)
   ((~~~ z (g g) (- -) (i i) ~~~))
   (oops)
   9))

(check-equal? (syntax-parse #'((yy 1 2) #:kw (yyy 3 4))
                [({~and {~or (y x …) :keyword}} …)
                 (list (?? (list y (?? x '-) …) 'oops) …)])
              '((yy 1 2) oops (yyy 3 4)))

(check-equal? (syntax-parse #'((y a b 3 d 5 f) #:kw (z g 8 i) #:kww)
                [({~and {~or (y {~and {~or x:id _}} …) :keyword}} …)
                 (list (?? (list y (?? x '-) …) 'oops) …)])
              '((y a b - d - f) oops (z g - i) oops))

(check-exn
 #rx"attribute contains an omitted element"
 (λ ()
   (syntax-parse #'((y a b 3 d 5 f) #:kw (z g 8 i) #:kww)
     [({~and {~or (y {~and {~or x:id _}} …) :keyword}} …)
      (list (?? x '-) … …)])))

(check-exn
 #rx"attribute contains an omitted element"
 (λ ()
   (syntax-parse #'((y a b 3 d 5 f) #:kw (z g 8 i) #:kww)
     [({~and {~or (y {~and {~or x:id _}} …) :keyword}} …)
      (define l (?if y (?? x '-) 'oops)) … …
      l])))