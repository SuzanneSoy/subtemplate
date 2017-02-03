#lang racket/base

(provide subscript-equal?
         drop-subscripts
         find-subscript-binders)

(require (for-template stxparse-info/current-pvars)
         racket/private/sc
         racket/function
         racket/list
         phc-toolkit/untyped
         racket/contract
         racket/string
         racket/syntax)

(define/contract (extract-subscripts id)
  (-> identifier? string?)
  (cadr (regexp-match #px".(_.+|[ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]*)$"
                     (symbol->string (syntax-e id)))))

(define/contract (string-replace* str from* to*)
  (->i ([str string?]
        [from* (listof string?)]
        [to* (from*)
             (and/c (listof string?)
                    (λ (to*) (= (length from*) (length to*))))])
       [range string?])
  (if (null? from*)
      str
      (string-replace* (string-replace str (car from*) (car to*))
                       (cdr from*)
                       (cdr to*))))
      

(define/contract (normalize-subscripts sub)
  (-> string? string?)
  (if (or (string=? sub "")
          (equal? (string-ref sub 0) #\_))
      sub
      (string-append
       "_"
       (string-replace* sub
                        (map symbol->string
                             '(ₐ ₑ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵣ ₛ ₜ ᵤ ᵥ ₓ ᵦ ᵧ ᵨ ᵩ ᵪ))
                        (map symbol->string
                             '(A E H I J K L M N O P R S T U V X β γ ρ ϕ χ))))))
  
(define/contract (subscript-equal? bound binder)
  (-> identifier? identifier? (or/c #f string?))
  (let* ([binder-subscripts (normalize-subscripts (extract-subscripts binder))]
         [bound-subscripts  (normalize-subscripts (extract-subscripts bound))])
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

(define (filter-current-pvars bound)
  (remove-duplicates
   (map (λ (pv+u) (cons (syntax-local-get-shadower (car pv+u))
                        (cdr pv+u)))
        (filter (compose (conjoin identifier?
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
   bound-identifier=?
   #:key car))

;; Or write it as:
#;(define (filter-current-pvars bound)
    (for/list ([binder (current-pvars+unique)]
               #:when (identifier? (car binder))
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

(define/contract (find-subscript-binders bound)
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
      (filter-current-pvars bound))
      
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
                                  (map cons
                                       (syntax->datum #'(binder …))
                                       depths))
                          bound
                          (syntax->list #'(binder …))))
      
    ;; FINAL RETURN (list of same-depth binders + their depth)
    (return (list bound
                  #'(binder …)
                  #'(unique-at-runtime-id …)
                  (car depths)))))
