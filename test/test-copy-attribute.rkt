#lang racket

(require subtemplate/copy-attribute
         stxparse-info/parse
         stxparse-info/parse/experimental/template
         phc-toolkit/untyped
         rackunit)

(define (to-datum x) (syntax->datum (datum->syntax #f x)))

;; Depth 2, no missing values
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 2 3] [4 5])
                   [((x …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?@ y …) … ((y …) …)])]))
                '(1 2 3 4 5 ((1 2 3) (4 5))))

  ;; shadowing the y in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 2 3] [4 5])
                   [((x …) … y)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?@ y …) … ((y …) …)])]))
                '(1 2 3 ((1 2 3))))

  ;; syntax? is #f (the leaves are still syntax though)
  (check-equal? (to-datum
                 (syntax-parse #'([1 2 3] [4 5])
                   [((x …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (attribute* y)]))
                '([1 2 3] [4 5]))

  ;; same as above, check that we have syntax at the leaves
  (check-match (syntax-parse #'([1 2 3] [4 5])
                 [((x …) …)
                  (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                  (attribute* y)])
               (list (list (? syntax?) ...) ...))

  ;; syntax? is #f (the leaves are still syntax though), use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'([1 2 3] [4 5])
                   [((x …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (template [(?@ y …) … ((y …) …)])]))
                '(1 2 3 4 5 ((1 2 3) (4 5))))

  ;; syntax? is #f, the leaves are NOT syntax.
  ;; Checks that (attribute* y) is not syntax either.
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y `((1 2 3) (4 5)) 2 #f)
                  (attribute* y))
                '([1 2 3] [4 5])))

;; Depth 2, missing values at depth 1
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 2 3] #:kw [4 5])
                   [({~and {~or #:kw (x …)}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?? (?@ y …) empty) … ((?? (y …) empty) …)])]))
                '(1 2 3 empty 4 5 ((1 2 3) empty (4 5))))

  ;; shadowing the y in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 2 3] #:kw [4 5])
                   [({~and {~or #:kw (x …)}} … y)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?? (?@ y …) empty) … ((?? (y …) empty) …)])]))
                '(1 2 3 empty ((1 2 3) empty)))

  ;; syntax? is #f (the leaves are still syntax though)
  (check-equal? (to-datum
                 (syntax-parse #'([1 2 3] #:kw [4 5])
                   [({~and {~or #:kw (x …)}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (attribute* y)]))
                '([1 2 3] #f [4 5]))

  ;; same as above, check that we have syntax at the leaves
  (check-match (syntax-parse #'([1 2 3] #:kw [4 5])
                 [({~and {~or #:kw (x …)}} …)
                  (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                  (attribute* y)])
               (list (list (? syntax?) ...) #f (list (? syntax?) ...)))

  ;; syntax? is #f (the leaves are still syntax though), use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'([1 2 3] #:kw [4 5])
                   [({~and {~or #:kw (x …)}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (template [(?? (?@ y …) empty) … ((?? (y …) empty) …)])]))
                '(1 2 3 empty 4 5 ((1 2 3) empty (4 5))))

  ;; syntax? is #f, the leaves are NOT syntax.
  ;; Checks that (attribute* y) is not syntax either.
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '((1 2 3) #f (4 5)) 2 #f)
                  (attribute* y))
                '([1 2 3] #f [4 5])))

;; Depth 2, missing values at depth 2
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 #:kw 3] [4 5])
                   [(({~and {~or #:kw x}} …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?@ (?? y empty) …) … (((?? y empty) …) …)])]))
                '(1 empty 3 4 5 ((1 empty 3) (4 5))))

  ;; shadowing the y in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'([1 #:kw 3] [4 5])
                   [(({~and {~or #:kw x}} …) … y)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template [(?@ (?? y empty) …) … (((?? y empty) …) …)])]))
                '(1 empty 3 ((1 empty 3))))

  ;; syntax? is #f (the leaves are still syntax though)
  (check-equal? (to-datum
                 (syntax-parse #'([1 #:kw 3] [4 5])
                   [(({~and {~or #:kw x}} …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (attribute* y)]))
                '([1 #f 3] [4 5]))

  ;; same as above, check that we have syntax at the leaves
  (check-match (syntax-parse #'([1 #:kw 3] [4 5])
                 [(({~and {~or #:kw x}} …) …)
                  (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                  (attribute* y)])
               (list (list (or #f (? syntax?)) ...) ...))

  ;; syntax? is #f (the leaves are still syntax though), use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'([1 #:kw 3] [4 5])
                   [(({~and {~or #:kw x}} …) …)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (template [(?@ (?? y empty) …) … (((?? y empty) …) …)])]))
                '(1 empty 3 4 5 ((1 empty 3) (4 5))))

  ;; syntax? is #f, the leaves are NOT syntax.
  ;; Checks that (attribute* y) is not syntax either.
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '((1 #f 3) (4 5)) 2 #f)
                  (attribute* y))
                '([1 #f 3] [4 5])))

;; Depth 1, missing values at depth 1
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 #:kw 3)
                   [({~and {~or #:kw x}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #t)
                    (template ({?? y empty} …))]))
                '(1 empty 3))

  ;; shadowing the y in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 #:kw 3 4)
                   [({~and {~or #:kw x}} … y)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #t)
                    (template ({?? y empty} …))]))
                '(1 empty 3))

  ;; syntax? is #f (the leaves are still syntax though)
  (check-equal? (to-datum
                 (syntax-parse #'(1 #:kw 3)
                   [({~and {~or #:kw x}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                    (attribute* y)]))
                '(1 #f 3))

  ;; same as above, check that we have syntax at the leaves
  (check-match (syntax-parse #'(1 #:kw 3)
                 [({~and {~or #:kw x}} …)
                  (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                  (attribute* y)])
               (list (or #f (? syntax?)) ...))

  ;; syntax? is #f (the leaves are still syntax though), use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'(1 #:kw 3)
                   [({~and {~or #:kw x}} …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                    (template ({?? y empty} …))]))
                '(1 empty 3))

  ;; syntax? is #f, the leaves are NOT syntax.
  ;; Checks that (attribute* y) is not syntax either.
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '(1 #f 3) 1 #f)
                  (attribute* y))
                '(1 #f 3))

  ;; syntax? is #f, compound values
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '((1 1 1) #f (3 (#t) #f)) 1 #f)
                  (attribute* y))
                '((1 1 1) #f (3 (#t) #f))))

;; Depth 1, no missing values
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 2 3)
                   [(x …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #t)
                    (template ({?? y empty} …))]))
                '(1 2 3))

  ;; shadowing the y in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 2 3 4)
                   [(x … y)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #t)
                    (template ({?? y empty} …))]))
                '(1 2 3))

  ;; syntax? is #f (the leaves are still syntax though)
  (check-equal? (to-datum
                 (syntax-parse #'(1 2 3)
                   [(x …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                    (attribute* y)]))
                '(1 2 3))

  ;; same as above, check that we have syntax at the leaves
  (check-match (syntax-parse #'(1 2 3)
                 [(x …)
                  (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                  (attribute* y)])
               (list (? syntax?) ...))

  ;; syntax? is #f (the leaves are still syntax though), use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'(1 2 3)
                   [(x …)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                    (template ({?? y empty} …))]))
                '(1 2 3))

  ;; syntax? is #f, the leaves are NOT syntax.
  ;; Checks that (attribute* y) is not syntax either.
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '(1 2 3) 1 #f)
                  (attribute* y))
                '(1 2 3))

  ;; syntax? is #f, compound values
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y '((1 1 1) 2 (3 (#t) #f)) 1 #f)
                  (attribute* y))
                '((1 1 1) 2 (3 (#t) #f))))

;; Depth 1, missing value at depth 0
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(#:kw)
                   [({~optional (x …)} #:kw)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #t)
                    (template {?? (y …) empty})]))
                'empty)

  ;; syntax? is #f, use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'(#:kw)
                   [({~optional (x …)} #:kw)
                    (copy-raw-syntax-attribute y (attribute* x) 1 #f)
                    (template {?? (y …) empty})]))
                'empty)

  ;; syntax? is #f, check with a raw attribute explicitly
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y #f 1 #f)
                  (attribute* y))
                #f)

  ;; syntax? is #f, check (in a template) with a raw attribute explicitly
  (check-equal? (syntax->datum
                 (let ()
                   (copy-raw-syntax-attribute y #f 1 #f)
                   (template {?? (y …) empty})))
                'empty))

;; Depth 2, missing value at depth 0
(begin
  ;; with just x in the pattern
  (check-equal? (syntax->datum
                 (syntax-parse #'(#:kw)
                   [({~optional ((x …) …)} #:kw)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
                    (template {?? ((y …) …) empty})]))
                'empty)

  ;; syntax? is #f, use it in a template
  (check-equal? (to-datum
                 (syntax-parse #'(#:kw)
                   [({~optional ((x …) …)} #:kw)
                    (copy-raw-syntax-attribute y (attribute* x) 2 #f)
                    (template {?? ((y …) …) empty})]))
                'empty)

  ;; syntax? is #f, check with a raw attribute explicitly
  (check-equal? (let ()
                  (copy-raw-syntax-attribute y #f 2 #f)
                  (attribute* y))
                #f)

  ;; syntax? is #f, check (in a template) with a raw attribute explicitly
  (check-equal? (syntax->datum
                 (let ()
                   (copy-raw-syntax-attribute y #f 2 #f)
                   (template {?? ((y …) …) empty})))
                'empty))
