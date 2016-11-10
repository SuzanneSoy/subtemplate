#lang racket
(require racket/require
         phc-toolkit/untyped
         racket/stxparam
         syntax/parse
         backport-template-pr1514/experimental/template
         ;syntax/parse/experimental/template
         ;syntax/parse/experimental/private/substitute
         syntax/id-table
         racket/syntax
         (for-syntax "patch-arrows.rkt"
                     syntax/parse
                     racket/private/sc
                     racket/syntax
                     racket/list
                     racket/function
                     phc-toolkit/untyped
                     syntax/strip-context
                     srfi/13
                     (subtract-in racket/string srfi/13)
                     syntax/contract
                     racket/contract))

(provide (rename-out [new-syntax-parse syntax-parse]
                     [new-syntax-parser syntax-parser]
                     [new-syntax-case syntax-case])
         ;define-unhygienic-template-metafunction
         subtemplate
         quasisubtemplate)

(begin-for-syntax (struct derived ()))
(define-syntax-parameter maybe-syntax-pattern-variable-ids '())
(define empty-pvar-values '())
(define-syntax-parameter pvar-values-id (make-rename-transformer
                                         #'empty-pvar-values))

(begin-for-syntax
  (define/contract (split-colon sym)
    (-> symbol? (cons/c symbol? (listof symbol?)))
    (cons sym
          (map string->symbol
               (string-split (symbol->string sym)
                             ":")))))

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
                       (append-map split-colon _)
                       (remove-duplicates)))
          (syntax-parameter-value
           #'maybe-syntax-pattern-variable-ids)));)

(begin-for-syntax
  (define/contract (wrap-with-parameterize lctx new-whole-form rest)
    (-> identifier? syntax? syntax? syntax?)
    (quasisyntax/top-loc lctx
      (let ()
        #,(patch-arrows
           ;; HERE insert a hash table, to cache the uses of derived pvars.
           ;; Lifting the define-temp-ids is not likely to work, as they
           ;; need to define syntax pattern variables so that other macros
           ;; can recognize them. Instead, we only lift the values, but still
           ;; do the bindings around the subtemplate.
           #`(let ([the-pvar-values (cons (make-hash) pvar-values-id)])
               (syntax-parameterize ([maybe-syntax-pattern-variable-ids
                                      #,(new-scope rest lctx)]
                                     [pvar-values-id (make-rename-transformer
                                                      #'the-pvar-values)])
                 #,new-whole-form)))))))

(begin-for-syntax
  (define/contract (simple-wrap-with-parameterize new-form-id)
    (-> identifier? (-> syntax? syntax?))
    (λ/syntax-case (self . rest) ()
      (wrap-with-parameterize #'self #`(#,new-form-id . rest) #'rest))))

(define-syntax new-syntax-parse
  (simple-wrap-with-parameterize #'syntax-parse))

(define-syntax new-syntax-case
  (simple-wrap-with-parameterize #'syntax-case))

(define-syntax (new-syntax-parser stx)
  (syntax-case stx ()
    [(self . rest)
     (quasisyntax/top-loc #'self
       (λ (stx2)
         #,(wrap-with-parameterize #'self
                                   #'((syntax-parser . rest) stx2)
                                   #'rest)))]))

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
                        [binder (in-value (datum->syntax lctx binder-sym #f))]
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
              (list binder scope-depth)))
          (if (null? found-here)
              recur-found
              (append found-here recur-found)))))

  (define/contract (find-subscript-binder2 bound)
    (-> identifier?
        (or/c #f (list/c identifier?                     ;; bound
                         (syntax/c (listof identifier?)) ;; binders
                         (syntax/c (listof identifier?)) ;; max-binders
                         exact-nonnegative-integer?      ;; ellipsis-depth
                         exact-nonnegative-integer?      ;; scope-depth
                         syntax?)))                      ;; check-ellipsis-count
    (define scopes (syntax-parameter-value #'maybe-syntax-pattern-variable-ids))
    (define/with-syntax ([binder scope-depth] …)
      (find-subscript-binder2a bound ;; TODO: check this is okay (should be).
                               scopes
                               bound
                               0))
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
          (define max-scope-depth (apply max (syntax->datum #'(scope-depth …))))
          (define max-binders
            (sort (map car
                       (filter (λ (bs) (= (syntax-e (cdr bs)) max-scope-depth))
                               (stx-map syntax-e #'([binder . scope-depth] …))))
                  symbol<?
                  #:key syntax-e))
          (list bound
                #'(binder …)
                #`#,max-binders
                (car depths)
                max-scope-depth
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
          (let ([binders+info (find-subscript-binder2 #'id)])
            (when binders+info
              (set! acc (cons binders+info acc)))
            #'id)]
      [other (rec #'other)]))
  ;; process the syntax, extract the derived bindings into acc
  (fold-syntax fold-process #'tmpl)
  ;; define the result, which looks like (template . tmpl) or
  ;; like (quasitemplate . tmpl)
  (define result
    (quasisyntax/top-loc #'self
      (#,tmpl-form
       . tmpl)))
  ;; Make sure that we remove duplicates, otherwise we'll get errors if we
  ;; define the same derived id twice.
  (define/with-syntax ([bound binders
                              max-binders
                              depth
                              scope-depth
                              check-ellipsis-count] …)
    (remove-duplicates acc #:key car))

  #`(let ()
      (derive bound binders max-binders depth scope-depth)
      …
      (let ()
        ;; no-op, just to raise an error when they are incompatible
        #'(check-ellipsis-count …)
        ;; actually call template or quasitemplate
        #,result)))

(define-syntax subtemplate (sub*template #'template))
(define-syntax quasisubtemplate (sub*template #'quasitemplate))



(define-syntax/case (derive bound binders max-binders stx-depth stx-scope-depth)
  ()
  ;; TODO: shouldn't it be called in the first place?
  (if (syntax-pattern-variable? (syntax-local-value #'bound (thunk #f)))
      #'(begin)
      #'(derive2 bound binders max-binders stx-depth stx-scope-depth)))

(define-syntax/case (derive2 bound
                             binders
                             (max-binder0 . max-binders)
                             stx-depth
                             stx-scope-depth) ()
  (define depth (syntax-e #'stx-depth))
  (define/with-syntax bound-ddd (nest-ellipses #'bound depth))
  (define/with-syntax tmp-id (format-id #'here "~a/~a" #'max-binder0 #'bound))
  (define/with-syntax tmp-str (datum->syntax #'tmp-id (symbol->string
                                                       (syntax-e #'tmp-id))))
  (define/with-syntax tmp-ddd (nest-ellipses #'tmp-id depth))
  (define/with-syntax binder-ddd (nest-ellipses #'max-binder0 depth))

  ;; Draw arrows in DrRacket.
  (with-arrows
   (define subscripts (subscript-equal? #'bound #'max-binder0))
   (define bound-id-str (identifier->string #'bound))
   (for ([max-binder (in-list (syntax->list #'(max-binder0 . max-binders)))])
     (define binder-id-str (identifier->string max-binder))
     (record-sub-range-binders! (vector #'bound
                                        (- (string-length bound-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)
                                        max-binder
                                        (- (string-length binder-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts))))
   #;(define binder0-id-str (identifier->string #'max-binder0))
   #;(record-sub-range-binders! (vector #'bound
                                        (- (string-length bound-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)
                                        #'max-binder0
                                        (- (string-length binder0-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)))
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
            (define cached (hash-ref! (list-ref pvar-values-id
                                                stx-scope-depth)
                                      'bound
                                      #'tmp-ddd))
            (define/with-syntax bound-ddd cached)
            (define-syntax #,(format-id #'bound "  is-derived-~a  " #'bound)
              (derived)))))


#|
(require syntax/parse/experimental/private/substitute)
;; Not very clean, but syntax/parse/experimental/template should export it :-(
(define (stolen-current-template-metafunction-introducer)
  ((eval #'current-template-metafunction-introducer
         (module->namespace 'syntax/parse/experimental/private/substitute))))

;; Note: define-unhygienic-template-metafunction probably only works correctly
;; when the metafunction is defined in the same file as it is used. The macro
;; which is built using that or other metafunctions can be used anywhere,
;; though. This is because we use a hack to guess what the old-mark from
;; syntax/parse/experimental/private/substitute is.
(define-syntax (define-unhygienic-template-metafunction xxx)
  (syntax-case xxx ()
    [(mee (name stx) . code)
     (datum->syntax
      #'mee
      `(define-template-metafunction (,#'name ,#'tmp-stx)
         (syntax-case ,#'tmp-stx ()
           [(self . _)
            (let* ([zero (datum->syntax #f 'zero)]
                   [normal ((,#'stolen-current-template-metafunction-introducer) (quote-syntax here))
                           #;(syntax-local-introduce
                              (syntax-local-get-shadower
                               (datum->syntax #f 'shadower)))]
                   [+self (make-syntax-delta-introducer normal zero)]
                   [+normal (make-syntax-delta-introducer normal zero)]
                   [mark (make-syntax-delta-introducer (+normal #'self 'flip)
                                                       zero)]
                   [,#'stx (syntax-local-introduce (mark ,#'tmp-stx 'flip))])
              (mark (syntax-local-introduce (let () . ,#'code))))])))]))
|#