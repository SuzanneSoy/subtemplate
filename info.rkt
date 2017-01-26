#lang info
(define collection "subtemplate")
(define deps '("base"
               "rackunit-lib"
               "backport-template-pr1514"
               "phc-toolkit"
               "srfi-lite-lib"
               "stxparse-info"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/subtemplate.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
