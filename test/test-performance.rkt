#lang racket

(require subtemplate/override)

#;(time
   (syntax-case #'((((0 1 2 3 4 5 6 7 8 9)))) ()
     [((((a b c d e f g h i j) …) …) …)
      #'(a … … …)]))

#;(time
   (syntax-case #'((((0 1 2 3 4 5 6 7 8 9)))) ()
     [((((a b c d e f g h i j) …) …) …)
      (list #'a … … …)]))

(time
 (syntax-case #'(((0 1 2 3 4 5 6 7 8 9))) ()
   [(((a b c d e f g h i j) …) …)
    (list #'a … …)]))