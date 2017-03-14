#lang racket
(require subtemplate/private/copy-attribute
         stxparse-info/parse
         stxparse-info/parse/experimental/template
         phc-toolkit/untyped)

(syntax->datum
 (syntax-parse #'([1 2 3] #:kw [4 5])
   [({~and {~or #:kw (x …)}} …)
    (copy-raw-syntax-attribute y (attribute* x) 2 #t)
    (template [(?? (?@ y …) empty) …])]))