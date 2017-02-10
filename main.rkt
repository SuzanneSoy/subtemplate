#lang racket/base
(require racket/require
         subtemplate/private/ddd-forms
         (subtract-in racket/syntax stxparse-info/case)
         stxparse-info/case
         stxparse-info/parse
         subtemplate/private/unsyntax-preparse
         subtemplate/private/top-subscripts
         (except-in stxparse-info/parse/experimental/template
                    template
                    quasitemplate
                    template/loc
                    quasitemplate/loc
                    ?@
                    ??)
         (only-in racket/base [... …])
         (only-in stxparse-info/parse [...+ …+]))
(provide
 ;; ddd-forms
 ?@ ?@@ ?? ?attr ?cond ?if begin let #%app #%intdef-begin
 ;; racket/syntax ∖ stxparse-info/case
 (all-from-out racket/syntax)
 ;; stxparse-info/case
 (all-from-out stxparse-info/case)
 ;; stxparse-info/parse
 (all-from-out stxparse-info/parse)
 ;; stxparse-info/parse/experimental/template
 (all-from-out stxparse-info/parse/experimental/template)
 ;; subtemplate/private/unsyntax-preparse
 (rename-out
  [template-ddd template]
  [subtemplate-ddd subtemplate]
  [quasitemplate-ddd quasitemplate]
  [quasisubtemplate-ddd quasisubtemplate])
 ;; subtemplate/private/top-subscripts
 ;; => #%top
 (all-from-out subtemplate/private/top-subscripts)
 ;; aliases
 …
 …+)