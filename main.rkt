#lang racket

(require racket/require
         phc-toolkit/untyped
         phc-toolkit/untyped-only/syntax-parse
         racket/stxparam
         stxparse-info/parse
         stxparse-info/case
         stxparse-info/current-pvars
         stxparse-info/parse/experimental/template
         (prefix-in - stxparse-info/parse/private/residual)
         (prefix-in dbg: stxparse-info/parse/private/runtime)
         syntax/id-table
         (subtract-in racket/syntax stxparse-info/case)
         (for-syntax "patch-arrows.rkt"
                     racket/format
                     stxparse-info/parse
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

(provide subtemplate
         quasisubtemplate)

(define derived-valvar-cache (make-weak-hash))

(begin-for-syntax
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

  (define/contract (drop-subscripts id)
    (-> identifier? identifier?)
    (let* ([str (symbol->string (syntax-e id))]
           [sub (extract-subscripts id)]
           [new-str (substring str 0 (- (string-length str)
                                        (string-length sub)))])
      (datum->syntax id (string->symbol new-str) id id)))

  (define/contract (nest-ellipses stx n)
    (-> syntax? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        stx
        #`(#,(nest-ellipses stx (sub1 n))
           (… …))))

  (define/contract (nest-map f last n)
    (-> (-> syntax? syntax?) syntax? exact-nonnegative-integer? syntax?)
    (if (= n 0)
        last
        (f (nest-map f last (sub1 n)))))

  (define/contract (find-subscript-binder bound)
    (-> identifier?
        (or/c #f (list/c identifier?                     ; bound
                         (syntax/c (listof identifier?)) ; binders
                         (syntax/c (listof identifier?)) ; unique-at-runtime ids
                         exact-nonnegative-integer?)))   ; ellipsis-depth

    (let/cc return
      ;; EARLY RETURN (already a pattern variable)
      (when (syntax-pattern-variable?
             (syntax-local-value bound (thunk #f)))
        (return #f))
      
      (define/with-syntax ([binder . unique-at-runtime-id] …)
        (filter (compose (conjoin identifier?
                                  (negate id-is-derived-valvar?)
                                  (λ~> (syntax-local-value _ (thunk #f))
                                       syntax-pattern-variable?)
                                  ;; force call syntax-local-value to prevent
                                  ;; ambiguous bindings, as syntax-local-value
                                  ;; triggers an error for those.
                                  ;; Must be done before the free-identifier=?
                                  ;; which just returns #false
                                  (λ~> (datum->syntax _ (syntax-e bound))
                                       (syntax-local-value _ (thunk #f))
                                       (thunk* #t)) ;; ok if no error.
                                  (λ~> (datum->syntax _ (syntax-e bound))
                                       (free-identifier=? _ bound))
                                  (λ~> (subscript-equal? bound _)))
                         car)
                (current-pvars+unique)))

      ;; Or write it as:

      #;(define/with-syntax ([binder . unique-at-runtime] …)
          (for/list ([binder (current-pvars)]
                      #:when (identifier? (car binder))
                      #:unless (id-is-derived-pvar? (car binder))
                      #:when (syntax-pattern-variable?
                              (syntax-local-value (car binder) (thunk #f)))
                      ;; force call syntax-local-value to prevent ambiguous
                      ;; bindings, as syntax-local-value triggers an error for
                      ;; those.
                      ;; Must be done before the free-identifier=? which just
                      ;; returns #false
                      #:when (begin
                               (syntax-local-value
                                (datum->syntax _ (syntax-e bound))
                                (thunk #f))
                               #t) ;; ok if no error.
                      #:when (free-identifier=? (datum->syntax (car binder)
                                                               (syntax-e bound))
                                                bound)
                      #:when (subscript-equal? bound (car binder)))
            binder))
      
      ;; EARLY RETURN (no candidate binders found)
      (when (stx-null? #'(binder …))
        (return #f))
      
      (define depths
        (stx-map (∘ syntax-mapping-depth syntax-local-value) #'(binder …)))
      
      ;; EARLY ERROR (inconsistent depths)
      (unless (or (< (length depths) 2) (apply = depths))
        (car depths)
        (raise-syntax-error 'subtemplate
                            (format "inconsistent depths: ~a"
                                    (syntax->list #'(binder …)))
                            bound))
      
      ;; FINAL RETURN (list of same-depth binders + their depth)
      (return (list bound
                    #'(binder …)
                    #'(unique-at-runtime-id …)
                    (car depths))))))

(define/contract (attribute-val/c depth [bottom-predicate any/c])
  (->* {exact-nonnegative-integer?} {flat-contract?} flat-contract?)
  (flat-named-contract
   (build-compound-type-name 'attribute-val/c depth bottom-predicate)
   (λ (l)
     (if (= depth 0)
         (or (eq? l #f) (bottom-predicate l))
         (or (eq? l #f)
             (and (list? l)
                  (andmap (attribute-val/c (sub1 depth)) l)))))))

;; ellipsis-count/c works with attributes too, including missing (optional)
;; elements in the lists, at any level.
(define/contract (ellipsis-count/c depth [bottom-predicate any/c])
  (->* {exact-nonnegative-integer?} {flat-contract?} flat-contract?)
  (flat-named-contract
   (build-compound-type-name 'ellipsis-count/c depth bottom-predicate)
   (λ (l*)
     (true?
      (and (list? l*)
           (let ([l* (filter identity l*)])
             (if (= depth 0)
                 (andmap bottom-predicate l*)
                 (let ([lengths (map length l*)])
                   (and (or (< (length lengths) 2) (apply = lengths))
                        (or (empty? l*)
                            (apply andmap
                                   (λ sublists
                                     ((ellipsis-count/c (sub1 depth)
                                                        bottom-predicate)
                                      sublists))
                                   l*)))))))))))

(define/contract (map-merge-stx-depth f l* depth)
  (->i {[f (-> (listof any/c) any/c)]
        [l* (depth) (ellipsis-count/c depth any/c)]
        [depth exact-nonnegative-integer?]}
       {result (depth l*)
               (λ (r) ((ellipsis-count/c depth) (cons r l*)))})
  (let ([l* (filter identity l*)])
    (if (= depth 0)
        (f l*)
        (if (empty? l*)
            #f
            (apply map
                   (λ sublists
                     (map-merge-stx-depth f
                                          sublists
                                          (sub1 depth)))
                   l*)))))

(define-for-syntax (sub*template self-form tmpl-form)
  (syntax-parser
    [(self {~optional {~and #:force-no-stxinfo force-no-stxinfo}}
           {~optkw #:props (prop:id ...)}
           ;; #: marks end of options (so that we can have implicit ?@ later)
           {~optional #:}
           tmpl)
     (unless (attribute force-no-stxinfo)
       (for ([sym (in-list '(syntax-parse define/syntax-parse syntax-parser
                              syntax-case define/with-syntax with-syntax))])
         (let ([shadower (syntax-local-get-shadower (datum->syntax #'self sym))]
               [good (datum->syntax #'here sym)])
           (when (or (not (identifier-binding shadower))
                     (not (free-identifier=? shadower good)))
             (raise-syntax-error self-form
                                 (~a sym  (if (identifier-binding shadower)
                                              (~a " resolves to the official "
                                                  sym ",")
                                              " seems undefined,")
                                     " but subtemplate needs the patched"
                                     " version from stxparse-info. Use (require"
                                     " stxparse-info/parse) and (require"
                                     " stxparse-info/case) to fix this. This"
                                     " message can be disabled with (" self-form
                                     " #:force-no-stxinfo …), if you know what"
                                     " you're doing."))))))
     
     (define acc '())

     ;; Finds identifiers of the form zᵢ, and return a list of existing xᵢ
     ;; bindings
     (define (fold-process stx rec)
       (syntax-case stx ()
         [(id . _) (and (identifier? #'id)
                        (free-identifier=? #'id #'unsyntax))
                   stx]
         [id (identifier? #'id)
             (let ([binders+info (find-subscript-binder #'id)])
               (when binders+info
                 (set! acc (cons binders+info acc)))
               #'id)]
         [other (rec #'other)]))
     ;; Process the syntax, extract the derived bindings into acc
     ;; Does not take zᵢ identifiers generated by template metafunctions into
     ;;   account for now.
     (fold-syntax fold-process #'tmpl)
  
     ;; define the result, which looks like (template . tmpl) or
     ;; like (quasitemplate . tmpl)
     (define result
       (quasisyntax/top-loc #'self
         (#,tmpl-form tmpl
                      #,@(if (attribute props) #'(#:props (prop ...)) #'()))))
     ;; Make sure that we remove duplicates, otherwise we'll get errors if we
     ;; define the same derived id twice.
     (define/with-syntax ([bound
                           (binder …)
                           unique-at-runtime-ids
                           ellipsis-depth]
                          …)
       (remove-duplicates acc bound-identifier=? #:key car))

     (define/with-syntax whole-form-id (generate-temporary 'whole-subtemplate))

     #`(let-values ()
         (define-values (whole-form-id) (quote-syntax #,this-syntax))
         (derive
          bound (binder …) unique-at-runtime-ids ellipsis-depth whole-form-id)
         …
         (let-values ()
           ;; check that all the binders for a given bound are compatible.
           ((ellipsis-count/c ellipsis-depth) (list (attribute* binder) …)) …
           ;; actually call template or quasitemplate
           #,result))]))

(define-syntax subtemplate
  (sub*template 'subtemplate #'template))
(define-syntax quasisubtemplate
  (sub*template 'quasisubtemplate #'quasitemplate))

(define/contract (multi-hash-ref! h keys to-set)
  ;; This assumes that the hash does not get mutated during the execution of
  ;; this function.
  (-> (and/c (hash/c symbol? any/c #:immutable #f) hash-weak?)
      (listof symbol?)
      any/c
      any/c)
  (define val (or (for/or ([k (in-list keys)]) (hash-ref h k #f))
                  to-set))
  (for ([k (in-list keys)]) (hash-ref! h k val))
  val)

(define formattable/c (or/c number? string? symbol? bytes?))

(define/contract
  (generate-nested-ids depth bound binder₀ format l* attribute-names whole-form)
  (->i {[depth exact-nonnegative-integer?]
        [bound identifier?]
        [binder₀ identifier?]
        [format (-> formattable/c string?)]
        [l* (depth) (listof (attribute-val/c depth))]
        [attribute-names (l*) (and/c (listof identifier?)
                                     (λ (a) (= (length l*) (length a))))]
        [whole-form syntax?]}
       #:pre (l* depth attribute-names whole-form bound)
       (if ((ellipsis-count/c depth) l*)
           #t
           (raise-syntax-error
            (syntax-case whole-form ()
              [(self . _) (syntax-e #'self)]
              [_ 'subtemplate])
            "incompatible ellipsis match counts for subscripted variables:"
            whole-form
            bound
            attribute-names))
       {result (depth l*)
               (and/c (attribute-val/c depth identifier?)
                      (λ (r) ((ellipsis-count/c depth) (cons r l*))))})

  
  (define (gen bottom*)
    (define v
      (let ([vs (filter-map (λ (v)
                              (cond [(formattable/c v) v]
                                    [(formattable/c (syntax-e v)) (syntax-e v)]
                                    [else #f]))
                            bottom*)])
        (if (empty? vs)
            (syntax-e (generate-temporary binder₀))
            (car vs))))
    (datum->syntax ((make-syntax-introducer) bound)
                   (string->symbol (format v))))

  (map-merge-stx-depth gen l* depth))

(define-syntax/case (derive bound
                            (binder₀ binderᵢ …)
                            (unique-at-runtime-idᵢ …)
                            ellipsis-depth
                            whole-form-id) ()
  (define depth (syntax-e #'ellipsis-depth))
  (define/with-syntax bound-ddd (nest-ellipses #'bound depth))
  (define/with-syntax tmp-id
    (format-id #'here "~a/~a" #'binder₀ (drop-subscripts #'bound)))
  (define/with-syntax tmp-str
    (datum->syntax #'tmp-id
                   (symbol->string
                    (syntax-e
                     (format-id #'here "~~a/~a" (drop-subscripts #'bound))))))
  (define/with-syntax tmp-ddd (nest-ellipses #'tmp-id depth))
  (define/with-syntax binder-ddd (nest-ellipses #'binder₀ depth))

  ;; Draw arrows in DrRacket.
  (with-arrows
   (define subscripts (subscript-equal? #'bound #'binder₀))
   (define bound-id-str (identifier->string #'bound))
   (for ([binder (in-list (syntax->list #'(binder₀ binderᵢ …)))])
     (define binder-id-str (identifier->string binder))
     (record-sub-range-binders! (vector #'bound
                                        (- (string-length bound-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)
                                        binder
                                        (- (string-length binder-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts))))
   #;(define binder0-id-str (identifier->string #'binder0))
   #;(record-sub-range-binders! (vector #'bound
                                        (- (string-length bound-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)
                                        #'binder0
                                        (- (string-length binder0-id-str)
                                           (string-length subscripts))
                                        (string-length subscripts)))
   (define/with-syntax temp-derived (generate-temporary #'bound))
   (define/with-syntax temp-valvar (generate-temporary #'bound))
   (define/with-syntax temp-cached (generate-temporary #'bound))
   ;; works only for syntax patterns, luckily that's all we need since we
   ;; produce a tree of (possibly missing) identifiers.
   (define/with-syntax copy-attribute-pattern
     ;; the ~and is important, to prevent the nested ~or from being treated as
     ;; an ellipsis-head pattern.
     (nest-map (λ (pat) #`{~or #f ({~and #,pat} (... ...))})
               #'{~or #f {~var bound id}}
               (syntax-e #'ellipsis-depth)))
   ;; HERE: cache the define-temp-ids in the free-id-table, and make sure
   ;; that we retrieve the cached ones, so that two subtemplate within the same
   ;; syntax-case or syntax-parse clause use the same derived ids.
   ;;
   ;; We mark specially those bindings bound by (derive …) so that they are
   ;; not seen as original bindings in nested subtemplates (e.g. with an
   ;; "unsyntax"), otherwise that rule may not hold anymore, e.g.
   ;; (syntax-parse #'(a b c)
   ;;   [(xᵢ …)
   ;;    (quasisubtemplate (yᵢ …
   ;;                       #,(quasisubtemplate zᵢ …) ;; must be from xᵢ, not yᵢ
   ;;                       zᵢ …))])
   ;; the test above is not exactly right (zᵢ will still have the correct
   ;; binding), but it gives the general idea.
   #`(begin ;(define-temp-ids #:concise tmp-str binder-ddd) ;;;;;;;;;;;;;;;;;;;TODO: should fuse all the binder-ddd, so that if any one is not #f for a sublist, that sublist is generated.
       ;; TODO: we should check that if the hash-table access worked,
       ;; any new pvars are compatible with the old ones on which the cache is
       ;; based (in the sense of "no new non-#f positions")
       (define temp-cached
         (free-id-table-ref! (multi-hash-ref! derived-valvar-cache
                                              (list unique-at-runtime-idᵢ
                                                    …)
                                              (make-free-id-table))
                             (quote-syntax bound)
                             (λ ()
                               (generate-nested-ids 'ellipsis-depth
                                                    (quote-syntax bound)
                                                    (quote-syntax binder₀)
                                                    (λ (v) (format tmp-str v))
                                                    (list (attribute* binder₀)
                                                          (attribute* binderᵢ)
                                                          …)
                                                    (list (quote-syntax binder₀)
                                                          (quote-syntax binderᵢ)
                                                          …)
                                                    whole-form-id))))
       #;(define-syntax temp-derived
         (derived-valvar (quote-syntax temp-cached)))
       #;(define-raw-attribute bound
         temp-valvar
         temp-cached ;temp-derived
         ellipsis-depth
         #t)
       ;(define temp-cached (attribute* binder₀))
       ;; manually creating the attribute with (make-attribute-mapping …)
       ;; works, but the attribute behaves in a bogus way when put inside
       ;; an (?@ yᵢ ...). I must be missing some step in the construction
       ;; of the attribute
       ;; TODO: I used make-attribute-mapping somewhere else, find it and change it !!!!!
       (define/syntax-parse copy-attribute-pattern temp-cached)
       (define-pvars bound))))
