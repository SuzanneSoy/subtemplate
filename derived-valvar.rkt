#lang racket/base

(provide valvar+props
         valvar+props-valvar
         valvar+props-properties
         pvar->valvar+props
         pvar-property)

(require racket/function
         racket/contract
         racket/private/sc
         (for-template (prefix-in - stxparse-info/parse/private/residual)))

;; Act like a syntax transformer, but which is recognizable via the
;; derived-pattern-variable? predicate.
(struct valvar+props (valvar properties)
  #:property prop:procedure
  (λ (self stx)
    #`(#%expression #,(valvar+props-valvar self))))

(define (pvar->valvar+props id)
  (define mapping (syntax-local-value id (thunk #f)))
  (and mapping ;; … defined as syntax
       (syntax-pattern-variable? mapping) ; and is a syntax pattern variable
       (let ()
         (define mapping-slv
           (syntax-local-value (syntax-mapping-valvar mapping) (thunk #f)))
         ;; either a mapping → attribute → derived,
         ;; or directly mapping → derived
         (or (and (-attribute-mapping? mapping-slv) ;; is an attribute
                  (let ([attribute-slv (syntax-local-value
                                        (-attribute-mapping-var mapping-slv)
                                        (thunk #f))])
                    ;; and the pvar's valvar is a derived
                    (and (valvar+props? attribute-slv)
                         attribute-slv))
             ;; or the pvar's valvar is derived
             (and (valvar+props? mapping-slv)
                  mapping-slv))))))

(define/contract (pvar-property id property)
  (-> identifier? symbol? any/c)
  (let ([valvar+props (valvar+props-properties id)])
    (and valvar+props
         (let ([properties (valvar+props-properties valvar+props)])
         (hash? properties)
         (immutable? properties)
         (hash-ref properties property #f)))))