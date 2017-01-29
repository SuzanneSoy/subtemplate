#lang racket/base

(provide (struct-out derived-valvar)
         id-is-derived-valvar?)

(require racket/function
         racket/private/sc
         (for-template (prefix-in - stxparse-info/parse/private/residual)))

;; Act like a syntax transformer, but which is recognizable via the
;; derived-pattern-variable? predicate.
(struct derived-valvar (valvar)
  #:property prop:procedure
  (λ (self stx)
    #`(#%expression #,(derived-valvar-valvar self))))

(define (id-is-derived-valvar? id)
  (define mapping (syntax-local-value id (thunk #f)))
  (and mapping ;; … defined as syntax
       (syntax-pattern-variable? mapping) ; and is a syntax pattern variable
       (let ()
         (define mapping-slv
           (syntax-local-value (syntax-mapping-valvar mapping) (thunk #f)))
         ;; either a mapping → attribute → derived,
         ;; or directly mapping → derived
         (or (and (-attribute-mapping? mapping-slv) ;; is an attribute
                  (derived-valvar? ;; and the pvar's valvar is a derived
                   (syntax-local-value (-attribute-mapping-var mapping-slv)
                                       (thunk #f))))
             ;; or the pvar's valvar is derived
             (derived-valvar? mapping-slv)))))