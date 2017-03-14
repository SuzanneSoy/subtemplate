#lang racket
(require subtemplate/private/copy-attribute
         stxparse-info/parse
         stxparse-info/parse/experimental/template
         phc-toolkit/untyped
         rackunit)

(check-not-exn
 (λ ()
   (syntax-parse #'([1 2 3] #:kw [4 5])
     [({~and {~or #:kw (x …)}} …)
      ;; The syntax? argument must be #f, not #t, when there are some optional
      ;; elements, otherwise an exception is raised.
      (copy-raw-syntax-attribute y (attribute* x) 2 #f)
      (template [(?? (?@ y …) empty) …])])))