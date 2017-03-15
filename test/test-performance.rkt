#lang racket

(require subtemplate/override)

(time
 (syntax-case #'((((0 1 2 3 4 5 6 7 8 9)))) ()
   [((((a b c d e f g h i j) …) …) …)
    #'(a … … …)]))

(time
 (syntax-case #'((((0 1 2 3 4 5 6 7 8 9)))) ()
   [((((a b c d e f g h i j) …) …) …)
    (list #'a … … …)]))

;; raco expand this-file.rkt | wc
;; shows that there are 160 extra words for each additional level of nesting
;; for the following code, between 2 and 5 (inclusive) levels of nesting.
(time
 (syntax-case #'((((((0 1 2 3 4 5 6 7 8 9)))))) ()
   [((((((a b c d e f g h i j) …) …) …) …) …)
    (list #'a … … … … …)]))