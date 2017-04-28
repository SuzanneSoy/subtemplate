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

  ;; find-subscript-binders detects the xᵢ pattern variables declared outside of
  ;; the #'bound syntax, for which a corresponding yᵢ occurs within the #'bound
  ;; syntax. Since #'bound should normally be a single identifier, this will in
  ;; effect check whether #'bound is of the form yᵢ, and if so whether a
  ;; corresponding pattern variable xᵢ is within scope. The ᵢ can be any
  ;; subscript, as long as it is the same for xᵢ and yᵢ.
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
      ;; If #'bound was not of the form yᵢ, or if we did not find a matching
      ;; pattern variable xᵢ, we fall back to the original #%top implementation
      (datum->syntax stx `(,#'#%top . ,#'bound))))