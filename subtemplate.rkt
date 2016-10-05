#lang racket
(require phc-toolkit/untyped
         racket/stxparam
         syntax/parse
         syntax/parse/experimental/template
         syntax/id-table
         racket/syntax
         (for-syntax syntax/parse
                     racket/private/sc
                     racket/syntax
                     racket/list
                     racket/function
                     phc-toolkit/untyped
                     srfi/13
                     racket/contract))

(provide (rename-out [new-syntax-parse syntax-parse]
                     [new-syntax-case syntax-case])
         subtemplate
         quasisubtemplate
         (for-syntax find-subscript-binder)) ;; for testing only

(define-syntax-parameter maybe-syntax-pattern-variable-ids '())
(define-syntax-parameter pvar-values-id #f)

(define-syntax/parse (new-syntax-parse . rest)
  (quasisyntax/top-loc (stx-car stx)
    (let ([the-pvar-values (make-free-id-table)])
      (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                             (cons '#,(remove-duplicates
                                       (filter symbol?
                                               (flatten
                                                (syntax->datum #'rest))))
                                   (syntax-parameter-value
                                    #'maybe-syntax-pattern-variable-ids))]
                            [pvar-values-id (make-rename-transformer
                                             #'the-pvar-values)])
        (syntax-parse . rest))
      #;(syntax-parse option …
          [clause-pat
           ;; HERE insert a hash table, to cache the uses
           ;; lifting the define-temp-ids is not likely to work, as they
           ;; need to define syntax pattern variables so that other macros
           ;; can recognize them. Instead, we only lift the values, but still
           ;; do the bindings around the subtemplate.
           #:do (define #,(lifted-scope (syntax-local-introduce #'pvar-values)
                                        'add)
                  (make-free-id-table))
           . clause-rest]
          …))))

(define-syntax/case (new-syntax-case . rest) ()
  (quasisyntax/top-loc (stx-car stx)
    (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                           (cons '#,(remove-duplicates
                                     (filter symbol?
                                             (flatten
                                              (syntax->datum #'rest))))
                                 (syntax-parameter-value
                                  #'maybe-syntax-pattern-variable-ids))])
      (syntax-case . rest))))

(begin-for-syntax
  (define/contract (string-suffix a b)
    (-> string? string? string?)
    (define suffix-length (string-suffix-length a b))
    (substring a
               (- (string-length a) suffix-length)))

  (define/contract (subscript-binder? bound binder)
    (-> identifier? identifier? (or/c #f string?))
    (and (syntax-pattern-variable?
          (syntax-local-value binder
                              (thunk #f)))
         (let* ([bound-string (symbol->string (syntax-e bound))]
                [binder-string (symbol->string (syntax-e binder))]
                [suffix (string-suffix bound-string binder-string)]
                [subs (regexp-match #px"[ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]+$" suffix)])
           (and subs (car subs)))))

  (define/contract (find-subscript-binder bound [fallback bound])
    (->* (identifier?) (any/c) (or/c identifier? any/c))
    (define result/scopes
      (for/list ([scope (in-list
                         (syntax-parameter-value
                          #'maybe-syntax-pattern-variable-ids))])
        (define result
          (for*/list ([sym (in-list scope)]
                      #:unless (string=? (symbol->string sym)
                                         (identifier->string bound))
                      [binder (in-value (datum->syntax bound sym))]
                      [subscripts (in-value (subscript-binder? bound
                                                               binder))]
                      #:when subscripts)
            (cons binder subscripts)))
        (and (not (null? result))
             (car (argmax (∘ string-length cdr) result)))))
    (or (ormap identity result/scopes)
        fallback))

  (define/contract (nest-ellipses id n)
    (-> identifier? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        id
        #`(#,(nest-ellipses id (sub1 n))
           (… …)))))

(define-syntax/case (derive bound binder stx-depth) ()
  (define depth (syntax-e #'stx-depth))
  (define/with-syntax bound-ddd (nest-ellipses #'bound depth))
  (define/with-syntax tmp-id (format-id #'here "~a/~a" #'binder #'bound))
  (define/with-syntax tmp-str (datum->syntax #'tmp-id (symbol->string
                                                       (syntax-e #'tmp-id))))
  (define/with-syntax tmp-ddd (nest-ellipses #'tmp-id depth))
  (define/with-syntax binder-ddd (nest-ellipses #'binder depth))
  ;; HERE: cache the define-temp-ids in the free-id-table, and make sure
  ;; that we retrieve the cached ones, so that two subtemplate within the same
  ;; syntax-case or syntax-parse clause use the same derived ids.
  ;; TODO: mark specially those bindings bound by (derive …) so that they are
  ;; not seen as original bindings in nested subtemplates (e.g. with an
  ;; "unsyntax"), otherwise that rule may not hold anymore, e.g.
  ;; (syntax-parse #'(a b c)
  ;;   [(xᵢ …)
  ;;    (quasisubtemplate (yᵢ …
  ;;                       #,(quasisubtemplate zᵢ …) ;; must be from xᵢ, not yᵢ
  ;;                       zᵢ …))])
  ;; the test above is not exactly right (zᵢ will still have the correct
  ;; binding), but it gives the general idea.

  #`(begin (define-temp-ids tmp-str binder-ddd)
           (define cached (free-id-table-ref! pvar-values-id
                                              (quote-syntax bound)
                                              #'tmp-ddd))
           (define/with-syntax bound-ddd cached)))

(define-for-syntax/case-args ((sub*template tmpl-form) (self . tmpl))
  (define acc '())
  (define result
    (quasisyntax/top-loc #'self
      (#,tmpl-form
       . #,(fold-syntax (λ (stx rec)
                          (if (identifier? stx)
                              (let ([binder (find-subscript-binder stx #f)])
                                (when binder
                                  (let ([depth (syntax-mapping-depth
                                                (syntax-local-value binder))])
                                    (set! acc `((,stx ,binder ,depth) . ,acc))))
                                stx)
                              (rec stx)))
                        #'tmpl))))
  ;; Make sure that we remove duplicates, otherwise we'll get errors if we use
  ;; the same derived id twice.
  (define/with-syntax ([bound binder depth] …)
    (remove-duplicates acc free-identifier=? #:key car))
  
  #`(let ()
      (derive bound binder depth)
      …
      #,result))

(define-syntax subtemplate (sub*template #'template))
(define-syntax quasisubtemplate (sub*template #'quasitemplate))