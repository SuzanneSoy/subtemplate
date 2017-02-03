#lang racket

(provide copy-raw-syntax-attribute
         attribute-val/c)

(require stxparse-info/current-pvars
         phc-toolkit/untyped
         stxparse-info/parse
         (for-syntax racket/contract
                     racket/syntax
                     phc-toolkit/untyped
                     racket/function
                     stxparse-info/parse))

(begin-for-syntax
  (define/contract (nest-map f last n)
    (-> (-> syntax? syntax?) syntax? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        last
        (f (nest-map f last (sub1 n))))))

(define/contract (attribute-val/c depth [bottom-predicate any/c])
  (->* {exact-nonnegative-integer?} {flat-contract?} flat-contract?)
  (flat-named-contract
   (build-compound-type-name 'attribute-val/c depth bottom-predicate)
   (λ (l)
     (if (= depth 0)
         (or (false? l) (bottom-predicate l))
         (or (false? l)
             (and (list? l)
                  (andmap (attribute-val/c (sub1 depth)) l)))))))

(struct wrapped (value))

(define (attribute-wrap val depth)
  (if (= depth 0)
      (wrapped val)
      (if val
          (map (λ (v) (attribute-wrap v (sub1 depth)))
               val)
          #f)))

;; manually creating the attribute with (make-attribute-mapping …)
;; works, but the attribute behaves in a bogus way when put inside
;; an (?@ yᵢ ...). I must be missing some step in the construction
;; of the attribute
(define-syntax/parse (copy-raw-syntax-attribute name:id
                                                attr-value:expr
                                                ellipsis-depth:nat
                                                syntax?:boolean)
  ;; the ~and is important, to prevent the nested ~or from being treated as
  ;; an ellipsis-head pattern.
  #:with nested (nest-map (λ (pat) #`{~or #f ({~and #,pat} (... ...))})
                          (if (syntax-e #'syntax?)
                              #'{~or #f name}
                              ;; Variable with empty name, so that the attribute
                              ;; gets exported without a prefix.
                              ;; Take care to keep the original srcloc,
                              ;; otherwise error messages lack the proper srcloc
                              #`{~or #f {~var #,(datum->syntax #'name
                                                               '||
                                                               #'name)
                                              extract-non-syntax}})
                          (syntax-e #'ellipsis-depth))
  (if (syntax-e #'syntax?)
      #'(begin
          (define/syntax-parse nested attr-value))
      #'(begin
          (define-syntax-class extract-non-syntax
            #:attributes (name)
            (pattern v
                     #:attr name (wrapped-value (syntax-e #'v))))
          (define/syntax-parse nested (attribute-wrap attr-value
                                                      ellipsis-depth)))))
