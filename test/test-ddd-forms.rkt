#lang racket

(require subtemplate/ddd-forms
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped)

;; case, let + begin, define
(check-equal? (syntax-case #'((1 2 3) (4 5)) ()
                [((x …) …)
                 (let ()
                   (begin
                     (define y (- (syntax-e #'x))) … …
                     y))])
              '((-1 -2 -3) (-4 -5)))

;; case, let + begin, define/with-syntax
(check-equal? (syntax->datum
               (syntax-case #'((1 2 3) (4 5)) ()
                 [((x …) …)
                  (let ()
                    (begin
                      (define/with-syntax y (- (syntax-e #'x))) … …
                      #'((y …) …)))]))
              '((-1 -2 -3) (-4 -5)))

;; case, let, define
(check-equal? (syntax-case #'((1 2 3) (4 5)) ()
                [((x …) …)
                 (let ()
                   (define y (- (syntax-e #'x))) … …
                   y)])
              '((-1 -2 -3) (-4 -5)))

;; case, let, define/with-syntax
(check-equal? (syntax->datum
               (syntax-case #'((1 2 3) (4 5)) ()
                 [((x …) …)
                  (let ()
                    (define/with-syntax y (- (syntax-e #'x))) … …
                    #'((y …) …))]))
              '((-1 -2 -3) (-4 -5)))

;; parse, let + begin, define
(check-equal? (syntax-parse #'((1 2 3) (4 5))
                [((x …) …)
                 (let ()
                   (begin
                     (define y (- (syntax-e #'x))) … …
                     y))])
              '((-1 -2 -3) (-4 -5)))

;; parse, let + begin, define/with-syntax
(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (4 5))
                 [((x …) …)
                  (let ()
                    (begin
                      (define/with-syntax y (- (syntax-e #'x))) … …
                      #'((y …) …)))]))
              '((-1 -2 -3) (-4 -5)))

;; parse, let, define
(check-equal? (syntax-parse #'((1 2 3) (4 5))
                [((x …) …)
                 (let ()
                   (define y (- (syntax-e #'x))) … …
                   y)])
              '((-1 -2 -3) (-4 -5)))

;; parse, let, define/with-syntax
(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (4 5))
                 [((x …) …)
                  (let ()
                    (define/with-syntax y (- (syntax-e #'x))) … …
                    #'((y …) …))]))
              '((-1 -2 -3) (-4 -5)))

;; parse, begin, define
(check-equal? (syntax-parse #'((1 2 3) (4 5))
                [((x …) …)
                 (begin
                   (define y (- (syntax-e #'x))) … …)
                 y])
              '((-1 -2 -3) (-4 -5)))

;; parse, begin, define/with-syntax
(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (4 5))
                 [((x …) …)
                  (begin
                    (define/with-syntax y (- (syntax-e #'x))) … …)
                  #'((y …) …)]))
              '((-1 -2 -3) (-4 -5)))

;; parse, directly in the body, define
(check-equal? (syntax-parse #'((1 2 3) (4 5))
                [((x …) …)
                 (define y (- (syntax-e #'x))) … …
                 y])
              '((-1 -2 -3) (-4 -5)))

;; parse, directly in the body, define/with-syntax
(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (4 5))
                 [((x …) …)
                  (define/with-syntax y (- (syntax-e #'x))) … …
                  #'((y …) …)]))
              '((-1 -2 -3) (-4 -5)))

;; #%app
(check-equal? (syntax-case #'([1 2 3] [a]) ()
                [([x …] [y …])
                 (vector (syntax-e #'x) … 'then (syntax-e #'y) …)])
              #(1 2 3 then a))

;; #%app, depth 2 → flat
(check-equal? (syntax-case #'(([1 2 3] [4 5 6]) [a]) ()
                [(([x …] …) [y …])
                 (vector (syntax-e #'x) … … 'then (syntax-e #'y) …)])
              #(1 2 3 4 5 6 then a))

;; #%app, depth 2 → nested
(check-equal? (syntax-case #'(([1 2 3] [4 5 6]) [a]) ()
                [(([x …] …) [y …])
                 (vector ((syntax-e #'x) …) … 'then (syntax-e #'y) …)])
              #((1 2 3) (4 5 6) then a))

;; #%app, with auto-syntax-e behaviour :)
(check-equal? (syntax-case #'([1 2 3] [a]) ()
                [([x …] [y …])
                 (vector x … 'then y …)])
              #(1 2 3 then a))

;; #%app, with auto-syntax-e behaviour, same variable iterated twice
(check-equal? (syntax-case #'([1 2 3] [a]) ()
                [([x …] [y …])
                 (vector x … 'then x …)])
              #(1 2 3 then 1 2 3))

;; #%app, depth 2 → flat, with auto-syntax-e behaviour :)
(check-equal? (syntax-case #'(([1 2 3] [4 5 6]) [a]) ()
                [(([x …] …) [y …])
                 (vector x … … 'then y …)])
              #(1 2 3 4 5 6 then a))

;; #%app, depth 2 → nested, with auto-syntax-e behaviour :)
(check-equal? (syntax-case #'(([1 2 3] [4 5 6]) [a]) ()
                [(([x …] …) [y …])
                 (vector (x …) … 'then y …)])
              #((1 2 3) (4 5 6) then a))

(check-equal? (syntax-parse #'(([1 2 3] [4 5 6]) [a])
                [(([x …] …) [y …])
                 (vector (x … …) 'then y …)])
              #((1 2 3 4 5 6) then a))

(check-equal? (syntax-parse #'(([1 2 3] [4 5 6]) [a])
                [(([x …] …) [y …])
                 (y …)])
              '(a))

(check-equal? (syntax-parse #'(([1 2 3] [4 5 6]) [a])
                [(([x …] …) [y …])
                 (x … …)])
              '(1 2 3 4 5 6))

;; Implicit (list _), could also be changed to an implicit (values).
(check-equal? (syntax-parse #'(([1 2 3] [4 5 6]) [a])
                [(([x …] …) [y …])
                 x … …])
              '(1 2 3 4 5 6))

;; TODO: expr … inside begin and let
(check-equal? (syntax-case #'((1 2 3) (4 5)) ()
                  [((x …) …)
                   (let ()
                     (list (length (syntax->list #'(x …)))
                           (+ (syntax-e #'x) 3) …)
                     …)])
                '([3 4 5 6]
                  [2 7 8]))

(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 x … …])
              '(1 2 3 4 5 6))
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 (x …) …])
              '((1 2 3) (4 5 6)))
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 ((list x) …) …])
              '(((1) (2) (3)) ((4) (5) (6))))
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 ((+ x 10) …) …])
              '((11 12 13) (14 15 16)))
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 (begin ((+ x 10) …) …)])
              '((11 12 13) (14 15 16)))
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                [([x …] …)
                 (define/with-syntax y (+ x 10)) … …
                 y … …])
              '(11 12 13 14 15 16))

;; Implicit apply with (+ y … …)
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                  [([x …] …)
                   (define/with-syntax y (+ x 10)) … …
                   (+ y … …)])
                81)

;; Implicit apply with (+ (* x 2) … …)
(check-equal? (syntax-parse #'([1 2 3] [4 5 6])
                  [([x …] …)
                   (+ (* x 2) … …)])
                42)

;; TODO: (define ) … … should register the variable with current-pvars.
#;(syntax-parse #'([1 2 3] [4 5 6])
    [([x …] …)
     (define y (+ x 10)) … …
     y … …])



#lang racket

(require subtemplate/ddd-forms
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         phc-toolkit/untyped)

(syntax-parse #'([1 2 3] #:kw [4 5 6])
    [({~and {~or [x …] #:kw}} …)
     ((x …) …)])