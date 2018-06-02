#lang racket

(provide copy-raw-syntax-attribute
         attribute-val/c)

(require version-case
         stxparse-info/current-pvars
         phc-toolkit/untyped
         stxparse-info/parse
         (for-syntax "optcontract.rkt"
                     racket/syntax
                     phc-toolkit/untyped
                     racket/function
                     stxparse-info/parse)
         (for-syntax (only-in auto-syntax-e/utils make-auto-pvar)))
(version-case
 [(version< (version) "6.90.0.24")
  (require (only-in stxparse-info/parse/private/residual
                    [make-attribute-mapping
                     compat-make-attribute-mapping]))]
 [else
  (require (only-in stxparse-info/parse/private/residual ;; must be an absolute path
                    check-attr-value
                    [attribute-mapping -make-attribute-mapping]))
  (define-for-syntax (compat-make-attribute-mapping valvar name depth syntax?)
    (-make-attribute-mapping
     valvar name depth (if syntax? #f (quote-syntax check-attr-value))))])

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
      (with-syntax ([vtmp (generate-temporary #'name)]
                    [stmp (generate-temporary #'name)])
        #'(begin
            (define vtmp attr-value);; TODO: if already an id, no need to copy it (unless the id is mutated)
            (define-syntax stmp
              (compat-make-attribute-mapping (quote-syntax vtmp)
                                      'name 'ellipsis-depth 'syntax?))
            (define-syntax name
              (make-auto-pvar 'ellipsis-depth (quote-syntax stmp)))
            (define-pvars name)))
      ;; TODO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ vvvvvvvvvvvvvvvvvvvvvvvvvv
      #'(begin
          (define-syntax-class extract-non-syntax
            #:attributes (name)
            (pattern v
                     #:attr name (wrapped-value (syntax-e #'v))))
          (define/syntax-parse nested (attribute-wrap attr-value
                                                      ellipsis-depth)))))
