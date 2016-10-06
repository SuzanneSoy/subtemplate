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
                     syntax/contract
                     racket/contract))

(provide (rename-out [new-syntax-parse syntax-parse]
                     [new-syntax-case syntax-case])
         subtemplate
         quasisubtemplate)

(begin-for-syntax (struct derived ()))
(define-syntax-parameter maybe-syntax-pattern-variable-ids '())
(define empty-pvar-values '())
(define-syntax-parameter pvar-values-id (make-rename-transformer
                                         #'empty-pvar-values))

(define-for-syntax (new-scope rest lctx)
  ;(wrap-expr/c
  ;#'(listof (cons/c identifier? (listof symbol?)))
  #`(cons (cons (quote-syntax #,(syntax-local-get-shadower
                                 (datum->syntax lctx
                                                'outer-lctx))
                              #:local)
                '#,(~> (syntax->datum rest)
                       flatten
                       (filter symbol? _)
                       (remove-duplicates)))
          (syntax-parameter-value
           #'maybe-syntax-pattern-variable-ids)));)

(define-syntax/parse (new-syntax-parse . rest)
  (quasisyntax/top-loc (stx-car stx)
    ;; HERE insert a hash table, to cache the uses of derived pvars.
    ;; Lifting the define-temp-ids is not likely to work, as they
    ;; need to define syntax pattern variables so that other macros
    ;; can recognize them. Instead, we only lift the values, but still
    ;; do the bindings around the subtemplate.
    (let ([the-pvar-values (cons (make-hash) pvar-values-id)])
      (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                             #,(new-scope #'rest (stx-car stx))]
                            [pvar-values-id (make-rename-transformer
                                             #'the-pvar-values)])
        (syntax-parse . rest)))))

(define-syntax/case (new-syntax-case . rest) ()
  (error "new-syntax-case not implemented yet")
  #;(quasisyntax/top-loc (stx-car stx)
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

  (define/contract (extract-subscripts id)
    (-> identifier? string?)
    (car (regexp-match #px"[ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]*$"
                       (symbol->string (syntax-e id)))))
  
  (define/contract (subscript-equal? bound binder)
    (-> identifier? identifier? (or/c #f string?))
    (let* ([binder-subscripts (extract-subscripts binder)]
           [bound-subscripts  (extract-subscripts bound)])
      (and (string=? binder-subscripts bound-subscripts)
           (not (string=? binder-subscripts ""))
           binder-subscripts)))

  (define/contract (find-subscript-binder2a lctx scopes bound scope-depth)
    (-> identifier?
        (listof (cons/c identifier? (listof symbol?)))
        identifier?
        exact-nonnegative-integer?
        (listof (list/c identifier? exact-nonnegative-integer?)))
    (if (null? scopes)
        '()
        (let ()
          (define outer-lctx (caar scopes))
          (define syms (cdar scopes))
          (define recur-found (find-subscript-binder2a outer-lctx
                                                       (cdr scopes)
                                                       bound
                                                       (add1 scope-depth)))
          (define found-here
            (for*/list ([binder-sym (in-list syms)]
                        [binder (in-value (datum->syntax lctx binder-sym))]
                        #;#:when #;(displayln (list bound binder
                                                    'pvar?= (syntax-pattern-variable?
                                                             (syntax-local-value binder (thunk #f)))
                                                    'derived?= (derived?
                                                                (syntax-local-value
                                                                 (format-id binder
                                                                            "  is-derived-~a  "
                                                                            binder)
                                                                 (thunk #f)))
                                                    (subscript-equal? bound
                                                                      binder)))
                        #:when (syntax-pattern-variable?
                                (syntax-local-value binder (thunk #f)))
                        #:when (not (derived?
                                     (syntax-local-value
                                      (format-id binder
                                                 "  is-derived-~a  "
                                                 binder)
                                      (thunk #f))))
                        [subscripts (in-value (subscript-equal? bound
                                                                binder))]
                        #:when subscripts)
              ;(displayln (list binder scope-depth))
              (list binder scope-depth)))
          ;(displayln (list* 'found-here=  bound '→ found-here))
          (if (null? found-here)
              recur-found
              (append found-here recur-found)))))

  (define/contract (find-subscript-binder2 bound)
    (-> identifier?
        (or/c #f (list/c identifier?                     ;; bound
                         (syntax/c (listof identifier?)) ;; bindings
                         exact-nonnegative-integer?      ;; ellipsis-depth
                         exact-nonnegative-integer?      ;; scope-depth
                         syntax?)))                      ;; check-ellipsis-count
    (define scopes (syntax-parameter-value #'maybe-syntax-pattern-variable-ids))
    (define/with-syntax ([binder scope-depth] …)
      (find-subscript-binder2a bound ;; TODO: check this is okay (should be).
                               scopes
                               bound
                               0))
    ;(displayln (syntax->datum #`(2 bound= #,bound 2a-result= [binder scope-depth] …)))
    (if (stx-null? #'(binder …))
        #f
        (let ()
          (define depths
            (stx-map (∘ syntax-mapping-depth syntax-local-value) #'(binder …)))
          (unless (or (< (length depths) 2) (apply = depths))
            (raise-syntax-error 'subtemplate
                                (format "inconsistent depths: ~a"
                                        (syntax->list #'(binder …)))
                                bound))
          ;; generate code to check that the bindings have all the same
          ;; ellipsis count
          (define/with-syntax check-ellipsis-count-ddd
            (nest-ellipses #'(binder …) (car depths)))
          (list bound
                #'(binder …)
                (car depths)
                (apply max (syntax->datum #'(scope-depth …)))
                #'check-ellipsis-count-ddd))))

  (define/contract (nest-ellipses stx n)
    (-> syntax? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        stx
        #`(#,(nest-ellipses stx (sub1 n))
           (… …)))))

(define-for-syntax/case-args ((sub*template tmpl-form) (self . tmpl))
  (define acc '())
  (define (fold-process stx rec)
    (syntax-case stx ()
      [(id . _) (and (identifier? #'id)
                     (free-identifier=? #'id #'unsyntax))
                stx]
      [id (identifier? #'id)
          (let ([binders (find-subscript-binder2 #'id)])
            (when binders
              ;(displayln (syntax->datum (datum->syntax #f binders)))
              (set! acc (cons binders acc)))
            #'id)]
      [other (rec #'other)]))
  (define result
    (quasisyntax/top-loc #'self
      (#,tmpl-form
       . #,(fold-syntax fold-process
                        #'tmpl))))
  ;; Make sure that we remove duplicates, otherwise we'll get errors if we
  ;; define the same derived id twice.
  (define/with-syntax ([bound (binder0 . binders)
                              depth
                              scope-depth
                              check-ellipsis-count] …)
    (remove-duplicates acc #:key car))

  #;(displayln (syntax->datum #'((derive2 bound binder0 (binder0 . binders) depth scope-depth)
                                 …)))
  
  #`(let ()
      (derive2 bound binder0 (binder0 . binders) depth scope-depth)
      …
      (let ()
        ;; no-op, just to raise an error when they are incompatible
        #'(check-ellipsis-count …)
        ;; actually call template or quasitemplate
        #,result)))

(define-syntax subtemplate (sub*template #'template))
(define-syntax quasisubtemplate (sub*template #'quasitemplate))





(define-syntax/case (derive2 bound binder0 binders stx-depth stx-scope-depth) ()
  (define/with-syntax bound-def #'bound #;(replace-context #'binder0 #'bound))
  (define depth (syntax-e #'stx-depth))
  (define/with-syntax bound-ddd (nest-ellipses #'bound-def depth))
  (define/with-syntax tmp-id (format-id #'here "~a/~a" #'binder0 #'bound-def))
  (define/with-syntax tmp-str (datum->syntax #'tmp-id (symbol->string
                                                       (syntax-e #'tmp-id))))
  (define/with-syntax tmp-ddd (nest-ellipses #'tmp-id depth))
  (define/with-syntax binder-ddd (nest-ellipses (replace-context #'bound #'binder0) ;; why oh why do I need replace-context here???
                                                depth))
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

  ;; TODO: shouldn't be called in the first place? ;; TODO: remove?
  (if (syntax-pattern-variable? (syntax-local-value #'bound (thunk #f)))
      #'(begin)
      #`(begin (define-temp-ids tmp-str binder-ddd)
               (define cached (hash-ref! (list-ref pvar-values-id
                                                   stx-scope-depth)
                                         'bound-def
                                         #'tmp-ddd))
               (define/with-syntax bound-ddd cached)
               (define-syntax #,(format-id #'bound
                                           "  is-derived-~a  "
                                           #'bound)
                 (derived)))))