#lang racket/base
(require (only-in "template-subscripts.rkt"
                  derive
                  ellipsis-count/c)
         phc-toolkit/untyped
         (for-syntax racket/base
                     racket/syntax
                     syntax/stx
                     (only-in racket/base [... …])
                     "subscripts.rkt"))

(provide (rename-out [top #%top]))

(define-syntax (top stx)
  (define/with-syntax bound (stx-cdr stx))

  (define binders+info (find-subscript-binders #'bound))

  (if binders+info
      (let ()
        (define/with-syntax [_bound
                             (binder …)
                             unique-at-runtime-ids
                             ellipsis-depth]
          binders+info)
  
        (define/with-syntax whole-form-id (generate-temporary 'whole-subtemplate))
  
        #'(let-values ()
            (define-values (whole-form-id) (quote-syntax #,this-syntax))
            (derive bound
                    (binder …)
                    unique-at-runtime-ids
                    ellipsis-depth
                    whole-form-id)
            (let-values ()
              ;; check that all the binders for a given bound are compatible.
              ((ellipsis-count/c ellipsis-depth) (list (attribute* binder) …))
              ;; actually call template or quasitemplate
              bound)))
      (datum->syntax stx `(,#'#%top . ,#'bound))))