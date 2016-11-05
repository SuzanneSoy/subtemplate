#lang racket
(require (rename-in "subtemplate.rkt"
                    [subtemplate syntax]
                    [quasisubtemplate quasisyntax]))
(provide (all-from-out "subtemplate.rkt"))