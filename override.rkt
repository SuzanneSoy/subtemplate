#lang racket
(require (rename-in subtemplate
                    [subtemplate syntax]
                    [quasisubtemplate quasisyntax]))
(provide (all-from-out subtemplate))