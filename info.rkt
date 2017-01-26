#lang info
(define collection "subtemplate")
(define deps '("base"
               "rackunit-lib"
               "backport-template-pr1514" ;; for the documentation only
               "phc-toolkit"
               "srfi-lite-lib"
               "stxparse-info"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/subtemplate.scrbl" () (parsing-library))))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
