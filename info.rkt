#lang info
(define collection "subtemplate")
(define deps '("base"
               "rackunit-lib"
               "backport-template-pr1514" ;; for the documentation only
               "phc-toolkit"
               "srfi-lite-lib"
               "stxparse-info"
               "alexis-util"
               "scope-operations"
               "auto-syntax-e"
               "version-case"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "scribble-math"))
(define scribblings '(("scribblings/subtemplate.scrbl" () (parsing-library))))
(define pkg-desc "Various enhancements on syntax templates")
(define version "1.2")
(define pkg-authors '("Suzanne Soy"))
