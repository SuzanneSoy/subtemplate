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
                     syntax/strip-context
                     srfi/13
                     racket/contract))

(provide (rename-out [new-syntax-parse syntax-parse]
                     [new-syntax-case syntax-case])
         subtemplate
         quasisubtemplate
         (for-syntax find-subscript-binder)) ;; for testing only

(define-syntax-parameter maybe-syntax-pattern-variable-ids '())
(define empty-pvar-values #f)
(define-syntax-parameter pvar-values-id (make-rename-transformer
                                         #'empty-pvar-values))

(define-syntax/parse (new-syntax-parse . rest)
  (quasisyntax/top-loc (stx-car stx)
    ;; HERE insert a hash table, to cache the uses
    ;; lifting the define-temp-ids is not likely to work, as they
    ;; need to define syntax pattern variables so that other macros
    ;; can recognize them. Instead, we only lift the values, but still
    ;; do the bindings around the subtemplate.
    (let ([the-pvar-values (or pvar-values-id (make-free-id-table))])
      ;; TODO: add a let before calling get-shadower.
      (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                             ((λ (x) #;(displayln x) x)
                              (cons (syntax->list
                                     (quote-syntax
                                      #,(~> (syntax->datum #'rest)
                                            flatten
                                            (filter symbol? _)
                                            (remove-duplicates)
                                            (map (λ (sym)
                                                   (syntax-local-get-shadower
                                                    (datum->syntax (stx-car stx)
                                                                   sym)
                                                    #t))
                                                 _))
                                      #:local))
                                    (syntax-parameter-value
                                     #'maybe-syntax-pattern-variable-ids)))]
                            [pvar-values-id (make-rename-transformer
                                             #'the-pvar-values)])
        (syntax-parse . rest)))))

(define-syntax/case (new-syntax-case . rest) ()
  (quasisyntax/top-loc (stx-car stx)
    (let ([the-pvar-values (or pvar-values-id (make-free-id-table))])
      (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                             (cons '#,(remove-duplicates
                                       (filter symbol?
                                               (flatten
                                                (syntax->datum #'rest))))
                                   (syntax-parameter-value
                                    #'maybe-syntax-pattern-variable-ids))]
                            [pvar-values-id (make-rename-transformer
                                             #'the-pvar-values)])
        (syntax-case . rest)))))

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

  (define/contract (subscript-equal? bound binder)
    (-> identifier? identifier? (or/c #f string?))
    (and (let* ([binder-string (symbol->string (syntax-e binder))]
                [bound-string (symbol->string (syntax-e bound))]
                [binder-s (regexp-match #px"[ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]*$" binder)]
                [bound-s  (regexp-match #px"[ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]*$" bound)])
           (equal? (car binder-s)
                   (car bound-s)))))

  (define/contract (derived? binder)
    (-> identifier? boolean?)
    (displayln 'TODO-89641)
    #f)

  (define/contract (find-subscript-binder2a scopes bound)
    (-> (listof (listof identifier?)) identifier? (listof identifier?))
    (if (null? scopes)
        '()
        (let ()
          (define scope (car scopes))
          (define recur-found (find-subscript-binder2a (cdr scopes) bound))
          (define found-here
            (for*/list ([binder (in-list scope)]
                        #:when (syntax-pattern-variable?
                                (syntax-local-value binder
                                                    (thunk #f)))
                        #:when (not (derived? binder))
                        [subscripts (in-value (subscript-equal? bound
                                                                binder))]
                        #:when subscripts)
              (list binder subscripts)))
          (if (null? found-here)
              recur-found
              (append found-here recur-found)))))

  (define/contract (find-subscript-binder2 scopes bound)
    (-> (listof (listof identifier?))
        identifier?
        (or/c #f (syntax/c (cons/c syntax? (listof identifier?)))))
    (define/with-syntax (binder …) (find-subscript-binder2a scopes bound))
    (if (stx-null? #'(binder …))
        #f
        (let ()
          (define depths
            (stx-map (∘ syntax-mapping-depth syntax-local-value) #'(binder …)))
          (unless (apply = depths)
            (raise-syntax-error 'subtemplate
                                (format "inconsistent depths: ~a"
                                        (syntax->list #'(binder …)))
                                bound))
          ;; generate code to check that the bindings have all the same
          ;; ellipsis count
          (define/with-syntax check-ellipsis-count-ddd
            (nest-ellipses #'(binder …) (car depths)))
          (values #'(check-ellipsis-count-ddd binder …)))))
  
  (define/contract (find-subscript-binder bound [fallback bound])
    (->* (identifier?) (any/c) (or/c identifier? any/c))
    (define result/scopes
      (for/list ([scope (in-list
                         (syntax-parameter-value
                          #'maybe-syntax-pattern-variable-ids))]
                 [scope-depth (in-naturals)])
        (define result
          (for*/list ([binder (in-list scope)]
                      #:when (displayln (list 'bound= (syntax-e bound)
                                              'binder= (syntax-e binder)
                                              'patvar? (syntax-pattern-variable? (syntax-local-value binder (thunk #f)))
                                              'free=?/shadowed
                                              (free-identifier=? binder
                                                                 (replace-context bound binder))))
                      #:unless (string=? (identifier->string binder)
                                         (identifier->string bound))
                      [subscripts (in-value (subscript-binder? bound
                                                               binder))]
                      #:when subscripts)
            (displayln (list 'bound= (syntax-e bound)
                             'binder= (syntax-e binder)
                             'patvar? (syntax-pattern-variable? (syntax-local-value binder (thunk #f)))
                             'free=?/shadowed
                             (free-identifier=? binder
                                                (replace-context bound binder))
                             subscripts))
            (cons binder subscripts)))
        (and (not (null? result))
             (syntax-local-introduce
              (car (argmax (∘ string-length cdr) result))))))
    (displayln (list* (syntax-e bound)
                      (map stx-e result/scopes)
                      (stx-e (ormap identity result/scopes))
                      (map (λ (v) (map syntax-e v))
                           (syntax-parameter-value
                            #'maybe-syntax-pattern-variable-ids))))
    (or (ormap identity result/scopes)
        fallback))

  (define/contract (nest-ellipses id n)
    (-> identifier? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        id
        #`(#,(nest-ellipses id (sub1 n))
           (… …)))))

(define-syntax/case (derive bound binder stx-depth) ()
  (define/with-syntax bound-def (replace-context #'binder #'bound))
  (define depth (syntax-e #'stx-depth))
  (define/with-syntax bound-ddd (nest-ellipses #'bound-def depth))
  (define/with-syntax tmp-id (format-id #'here "~a/~a" #'binder #'bound-def))
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

  ;; TODO: shouldn't be called in the first place?
  (if (syntax-pattern-variable? (syntax-local-value #'bound (thunk #f)))
      #'(begin)
      ((λ (x)
         #;(newline)
         ;(displayln (syntax->datum x))
         ;(displayln (list #'bound-def #'binder (hash-ref (syntax-debug-info #'bound-def) 'context)))
         x)
       #`(begin (define-temp-ids tmp-str binder-ddd)
                (define cached (free-id-table-ref! pvar-values-id
                                                   (quote-syntax bound-def #:local)
                                                   #'tmp-ddd))
                (define/with-syntax bound-ddd cached)))))

(define-for-syntax/case-args ((sub*template tmpl-form) (self . tmpl))
  (define acc '())
  (define (fold-process stx rec)
    (syntax-case stx ()
      [(id . _) (and (identifier? #'id)
                     (free-identifier=? #'id #'unsyntax))
                stx]
      [id (identifier? #'id)
          (let ([binder (find-subscript-binder #'id #f)])
            (when binder
              (let ([depth (syntax-mapping-depth
                            (syntax-local-value binder))])
                (set! acc `((,#'id ,binder ,depth) . ,acc))))
            #'id)]
      [other (rec #'other)]))
  (define result
    (quasisyntax/top-loc #'self
      (#,tmpl-form
       . #,(fold-syntax fold-process
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