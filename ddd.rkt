#lang racket

(provide ddd)

(require stxparse-info/current-pvars
         phc-toolkit/untyped
         subtemplate/copy-attribute
         (prefix-in - syntax/parse/private/residual)
         (for-syntax racket/contract
                     racket/syntax
                     phc-toolkit/untyped
                     racket/function
                     racket/struct
                     racket/list
                     syntax/id-set
                     racket/private/sc
                     scope-operations
                     racket/string))

(define-for-syntax x-pvar-scope (make-syntax-introducer))
(define-for-syntax x-pvar-present-marker (make-syntax-introducer))

(begin-for-syntax
  (define/contract (attribute-real-valvar attr)
    (-> identifier? (or/c #f identifier?))
    (define valvar
      (let ([slv (syntax-local-value attr (λ () #f))])
        (if (syntax-pattern-variable? slv)
            (let* ([valvar (syntax-mapping-valvar slv)]
                   [valvar-slv (syntax-local-value valvar (λ () #f))])
              (if (-attribute-mapping? valvar-slv)
                  (-attribute-mapping-var valvar-slv)
                  valvar))
            (raise-syntax-error
             'attribute*
             "not bound as an attribute or pattern variable"
             attr))))
    (if (syntax-local-value valvar (λ () #f)) ;; is it a macro-ish thing?
        (begin
          (log-warning
           (string-append "Could not extract the plain variable corresponding to"
                          " the pattern variable or attribute ~a"
                          (syntax-e attr)))
          #f)
        valvar)))

;; free-identifier=? seems to stop working on the valvars once we are outside of
;; the local-expand containing the let which introduced these valvars, therefore
;; we find which pvars were present within that let.
(define-syntax/case (detect-present-pvars (pvar …) body) ()
  (define/with-syntax (pvar-real-valvar …)
    (map syntax-local-introduce
         (stx-map attribute-real-valvar #'(pvar …))))

  (define/with-syntax expanded-body
    (local-expand #`(let-values ()
                      (quote-syntax #,(stx-map x-pvar-scope #'(pvar-real-valvar …)) #:local)
                      body)
                  'expression
                  '()))

  ;; Separate the valvars marked with x-pvar-scope, so that we know which valvar
  ;; to look for.
  (define-values (marked-real-valvar expanded-ids)
    (partition (λ (id) (all-scopes-in? x-pvar-scope id))
               (extract-ids #'expanded-body)))
  (define/with-syntax (real-valvar …)
    (map (λ (x-vv) (x-pvar-scope x-vv 'remove))
         marked-real-valvar))
  (define expanded-ids-set (immutable-free-id-set expanded-ids))

  ;; grep for valvars in expanded-body
  (define/with-syntax present-variables
    (for/vector ([x-vv (in-syntax #'(real-valvar …))]
                 [pv (in-syntax #'(pvar …))])
      (if (free-id-set-member? expanded-ids-set x-vv)
          #t
          #f)))
  
  #`(let-values ()
      (quote-syntax #,(x-pvar-present-marker #'present-variables))
      body))

(define (=* . vs)
  (if (< (length vs) 2)
      #t
      (apply = vs)))

(define (map#f* f attr-ids l*)
  (for ([l (in-list l*)]
        [attr-id (in-list attr-ids)])
  (when (eq? l #f)
    (raise-syntax-error (syntax-e attr-id)
                        "attribute contains an omitted element"
                        attr-id)))
  (unless (apply =* (map length l*))
    (raise-syntax-error 'ddd
                        "incompatible ellipis counts for template"))
  (apply map f l*))

(define-syntax/case (ddd body) ()
  (define/with-syntax (pvar …)
    (remove-duplicates
     (map syntax-local-get-shadower
          (map syntax-local-introduce
               (filter (conjoin identifier?
                                (λ~> (syntax-local-value _ (thunk #f))
                                     syntax-pattern-variable?)
                                attribute-real-valvar)
                       (reverse (current-pvars)))))
     bound-identifier=?))
  
  (define-temp-ids "~aᵢ" (pvar …))
  (define/with-syntax f
    #`(#%plain-lambda (pvarᵢ …)
                      (shadow pvar pvarᵢ) … ;; TODO: find a way to make the variable marked as "missing" if it is #f ? So that it triggers an error if used outside of ??
                      (let-values ()
                        (detect-present-pvars (pvar …)
                                              body))))

  ;; extract all the variable ids present in f
  (define/with-syntax expanded-f (local-expand #'f 'expression '()))

  (begin
    (define present-variables** (find-present-variables-vector #'expanded-f))
    (define present-variables*
      (and (vector? present-variables**)
           (vector->list present-variables**)))
    (unless ((listof (syntax/c boolean?)) present-variables*)
      (raise-syntax-error 'ddd
                          (string-append
                           "internal error: could not extract the vector of"
                           " pattern variables present in the body.")
                          stx))
    (define present-variables (map syntax-e present-variables*)))

  (unless (ormap identity present-variables)
    (raise-syntax-error 'ddd
                        "no pattern variables were found in the body"
                        stx))

  (begin
    ;; present?+pvars is a list of (list shadow? pv pvᵢ present? depth/#f)
    (define present?+pvars
      (for/list ([present? (in-list present-variables)]
                 [pv (in-syntax #'(pvar …))]
                 [pvᵢ (in-syntax #'(pvarᵢ …))])
        (if present?
            (match (attribute-info pv)
              [(list* _ _valvar depth _)
               (if (> depth 0)
                   (list #t pv pvᵢ #t depth)
                   (list #f pv pvᵢ #t depth))]) ;; TODO: detect shadowed bindings, if the pvar was already iterated on, raise an error (we went too deep).
            (list #f pv pvᵢ #f #f))))
    ;; Pvars which are iterated over
    (define/with-syntax ((_ iterated-pvar iterated-pvarᵢ _ _) …)
      (filter car present?+pvars))

    (when (stx-null? #'(iterated-pvar …))
      (no-pvar-to-iterate-error present?+pvars))
    
    ;; If the pvar is iterated, use the iterated pvarᵢ 
    ;; otherwise use the original (attribute* pvar)
    (define/with-syntax (filling-pvar …)
      (map (match-λ [(list #t pv pvᵢ #t _) pvᵢ]
                    [(list #f pv pvᵢ #t _) #`(attribute* #,pv)]
                    [(list #f pv pvᵢ #f _) #'#f])
           present?+pvars)))

  #'(map#f* (λ (iterated-pvarᵢ …)
              (expanded-f filling-pvar …))
            (list (quote-syntax iterated-pvar)
                  …)
            (list (attribute* iterated-pvar)
                  …)))

(define-syntax/case (shadow pvar new-value) ()
  (match (attribute-info #'pvar '(pvar attr))
    [`(attr ,valvar ,depth ,_name ,syntax?)
     #`(copy-raw-syntax-attribute pvar
                                  new-value
                                  #,(max 0 (sub1 depth))
                                  #,syntax?)]
    [`(pvar ,valvar ,depth)
     #`(copy-raw-syntax-attribute pvar
                                  new-value
                                  #,(max 0 (sub1 depth))
                                  #t)
     #;#`(define-raw-syntax-mapping pvar
         tmp-valvar
         new-value
         #,(sub1 depth))]))

(define-for-syntax (extract-ids/tree e)
  (cond
    [(identifier? e) e]
    [(syntax? e) (extract-ids/tree (syntax-e e))]
    [(pair? e) (cons (extract-ids/tree (car e)) (extract-ids/tree (cdr e)))]
    [(vector? e) (extract-ids/tree (vector->list e))]
    [(hash? e) (extract-ids/tree (hash->list e))]
    [(prefab-struct-key e) (extract-ids/tree (struct->list e))]
    [else null]))

(define-for-syntax (extract-ids e)
  (flatten (extract-ids/tree e)))

(define-for-syntax (find-present-variables-vector e)
  (cond
    [(and (syntax? e)
          (vector? (syntax-e e))
          (all-scopes-in? x-pvar-present-marker e))
     (syntax-e e)]
    [(syntax? e) (find-present-variables-vector (syntax-e e))]
    [(pair? e) (or (find-present-variables-vector (car e))
                   (find-present-variables-vector (cdr e)))]
    [(vector? e) (find-present-variables-vector (vector->list e))]
    [(hash? e) (find-present-variables-vector (hash->list e))]
    [(prefab-struct-key e) (find-present-variables-vector (struct->list e))]
    [else #f]))

(define-for-syntax (no-pvar-to-iterate-error present?+pvars)
  (raise-syntax-error
   'ddd
   (string-append
    "no pattern variables with depth > 0 were found in the body\n"
    "  pattern varialbes present in the body:\n"
    "   "
    (string-join
     (map (λ (present?+pvar)
            (format "~a at depth ~a"
                    (syntax-e (second present?+pvar))
                    (fifth present?+pvar)))
          (filter fourth present?+pvars))
     "\n   "))))