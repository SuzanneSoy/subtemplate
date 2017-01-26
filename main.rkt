#lang racket

(require racket/require
         phc-toolkit/untyped
         racket/stxparam
         stxparse-info/parse
         stxparse-info/current-pvars
         stxparse-info/parse/experimental/template
         syntax/id-table
         racket/syntax
         (for-syntax "patch-arrows.rkt"
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
         (syntax-pattern-variable? mapping) ;; and is a syntax pattern variable
         (derived-valvar? ;; and the pvar's valvar is derived
          (syntax-local-value (syntax-mapping-valvar mapping)
                              (thunk #f)))))

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

  (define/contract (find-subscript-binder bound)
    (-> identifier?
        (or/c #f (list/c identifier?                     ; bound
                         (syntax/c (listof identifier?)) ; binders
                         (syntax/c (listof identifier?)) ; unique-at-runtime ids
                         exact-nonnegative-integer?      ; ellipsis-depth
                         syntax?)))                      ; check-ellipsis-count
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
      
      ;; generate code to check that the bindings have all the same
      ;; ellipsis count, by simply generating a dummy syntax object, with
      ;; all the given binders nested under the right number of ellipses.
      (define/with-syntax check-ellipsis-count-ddd
        (nest-ellipses #'(binder …) (car depths)))

      ;; FINAL RETURN (list of same-depth binders + their depth)
      (list bound
            #'(binder …)
            #'(unique-at-runtime-id …)
            (car depths)
            #'check-ellipsis-count-ddd))))

(define-for-syntax/case-args ((sub*template tmpl-form) (self . tmpl))
  (define acc '())

  ;; Finds identifiers of the form zᵢ, and return a list of existing xᵢ bindings
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
      (#,tmpl-form . tmpl)))
  ;; Make sure that we remove duplicates, otherwise we'll get errors if we
  ;; define the same derived id twice.
  (define/with-syntax ([bound
                        binders
                        unique-at-runtime-ids
                        ellipsis-depth
                        check-ellipsis-count]
                       …)
    (remove-duplicates acc #:key car))

  #`(let ()
      (derive bound binders unique-at-runtime-ids ellipsis-depth)
      …
      (let ()
        ;; no-op, just to raise an error when they are incompatible
        #'(check-ellipsis-count …) ;; TODO: allow #f values for ~optional in syntax/parse ;;;;;;;;;;;;;;
        ;; actually call template or quasitemplate
        #,result)))

(define-syntax subtemplate (sub*template #'template))
(define-syntax quasisubtemplate (sub*template #'quasitemplate))

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

(define/contract ((stx-list*+stx/c depth) l)
  (-> exact-nonnegative-integer? (-> any/c boolean?))
  (if (= depth 0)
      (syntax? l)
      (and (syntax? l)
           (syntax->list l)
           (andmap (λ (lᵢ) ((stx-list*+stx/c (sub1 depth)) lᵢ))
                   (syntax->list l)))))

(define/contract ((list*+stx/c depth) l)
  (-> exact-nonnegative-integer? (-> any/c boolean?))
  (if (= depth 0)
      (syntax? l)
      (and (list? l)
           (andmap (λ (lᵢ) ((list*+stx/c (sub1 depth)) lᵢ))
                   l))))

(define/contract (destructure-stx-list* l depth)
  (->i ([l (depth) (stx-list*+stx/c depth)]
        [depth exact-nonnegative-integer?])
       [range (depth) (list*+stx/c depth)])
  (if (= depth 0)
      l
      (stx-map (λ (lᵢ) (destructure-stx-list* lᵢ (sub1 depth)))
               l)))

(define-syntax/case (derive bound
                            (binder₀ binderᵢ …)
                            (unique-at-runtime-idᵢ …)
                            ellipsis-depth) ()
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
   (define/with-syntax temp-cached (generate-temporary #'bound))
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
   #`(begin (define-temp-ids #:concise tmp-str binder-ddd)
            (define temp-cached
              (free-id-table-ref! (multi-hash-ref! derived-valvar-cache
                                                   '(unique-at-runtime-idᵢ …)
                                                   (make-free-id-table))
                                  (quote-syntax bound)
                                  (destructure-stx-list* #'tmp-ddd
                                                         'ellipsis-depth)))
            (define-syntax temp-derived
              (derived-valvar (quote-syntax temp-cached)))
            (define-syntax bound
              (make-syntax-mapping 'ellipsis-depth (quote-syntax temp-derived)))
            (define-pvars bound)))) 