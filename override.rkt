#lang racket
(require racket/require
         (rename-in subtemplate
                    [subtemplate syntax]
                    [quasisubtemplate quasisyntax])
         stxparse-info/parse
         (except-in stxparse-info/parse/experimental/template
                    template
                    quasitemplate
                    template/loc
                    quasitemplate/loc)
         stxparse-info/case
         (subtract-in racket/syntax stxparse-info/case))
(provide (all-from-out subtemplate
                       stxparse-info/parse
                       stxparse-info/parse/experimental/template
                       stxparse-info/case
                       racket/syntax))