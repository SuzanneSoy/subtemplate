#lang racket

;; Implementation of the (ddd e) macro, which iterates e over the syntax pattern
;; variables present in e. e should contain at least one syntax pattern variable
;; which is under ellipses.

(provide ddd ?? ?if ?cond ?attr ?@ ?@@
         splicing-list splicing-list-l splicing-list?)

(require stxparse-info/current-pvars
         phc-toolkit/untyped
         subtemplate/private/copy-attribute
         (prefix-in - syntax/parse/private/residual)
         racket/stxparam
         "lifted-variables-communication.rkt"
         (for-syntax "optcontract.rkt"
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
           (string-append "Could not extract the plain variable corresponding"
                          " to the pattern variable or attribute ~a"
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
                      (quote-syntax #,(stx-map x-pvar-scope
                                               #'(pvar-real-valvar …))
                                    #:local)
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
                 [pv (in-syntax #'(pvar …))]) ;; TODO: is this line used (I suspect both lists have the same length)?
      (if (free-id-set-member? expanded-ids-set x-vv)
          #t
          #f)))
  
  #`(let-values ()
      (quote-syntax #,(x-pvar-present-marker #'present-variables))
      ;; was "body", instead of "expanded-body". I think that was just a remnant
      ;; of a debugging session, so I changed it to "expanded-body".
      expanded-body))

(define (=* . vs)
  (if (< (length vs) 2)
      #t
      (apply = vs)))

;; map, with extra checks for missing elements (i.e. when one of the l* lists
;; is #f). If allow-missing? is specified, each #f list is replaced by
;; a stream of #f values. If all l* lists are #f, then there is no way to know
;; the number of iterations to make, so #f is returned (indicating that the
;; whole sequence is missing, instead of being merely empty.
(define (map#f* allow-missing? f attr-ids l*)
  (if allow-missing?
      (let ()
        (define non-#f-l* (filter identity l*))
        (unless (apply =* (map length non-#f-l*))
          (raise-syntax-error 'ddd
                              "incompatible ellipis counts for template"))
        (if (= (length non-#f-l*) 0)
            ;; If all lists are missing (#f), return a single #f value, indicating
            ;; that there are no elements to create the result list from.
            #f
            ;; Or should we use this?
            ;(apply f (map (const #f) l*))
            ;; i.e. just call the function once with every variable bound to #f,
            ;; i.e. missing.
          
            ;; replace the missing (#f) lists with a list of N #f values, where N
            ;; is the length of the other lists.
            (let* ([repeated-#f (map (const #f) (car non-#f-l*))]
                   [l*/repeated-#f (map (λ (l) (or l repeated-#f)) l*)])
              (apply map f l*/repeated-#f))))
      (let ()
        (for ([l (in-list l*)]
              [attr-id (in-list attr-ids)])
          (when (eq? l #f)
            (raise-syntax-error (syntax-e attr-id)
                                "attribute contains an omitted element"
                                attr-id)))
        (unless (apply =* (map length l*))
          (raise-syntax-error 'ddd
                              "incompatible ellipis counts for template"))
        (apply map f l*))))


(define-for-syntax (current-pvars-shadowers)
  (remove-duplicates
   (map syntax-local-get-shadower
        (map syntax-local-introduce
             (filter (conjoin identifier?
                              (λ~> (syntax-local-value _ (thunk #f))
                                   syntax-pattern-variable?)
                              attribute-real-valvar)
                     (reverse (current-pvars)))))
   bound-identifier=?))

(define-for-syntax (extract-present-variables expanded-form stx)
  ;; present-variables vector
  (define present-variables** (find-present-variables-vector expanded-form))
  (define present-variables*
    (and (vector? present-variables**)
         (vector->list present-variables**)))
  (unless ((listof (syntax/c boolean?)) present-variables*)
    (raise-syntax-error 'ddd
                        (string-append
                         "internal error: could not extract the vector of"
                         " pattern variables present in the body.")
                        stx))
  (define present-variables (map syntax-e present-variables*))

  ;; lifted variables
  (define lifted-variables
    (map (λ (id)
           (define prop (syntax-property id 'lifted-pvar))
           (unless ((cons/c symbol? stx-list?) prop)
             (raise-syntax-error 'ddd
                                 (string-append
                                  "internal error: 'lifted-pvar property was "
                                  "missing or not a (cons/c symbol? stx-list?)")
                                 stx))
           prop)
         (filter (λ (id) (all-scopes-in? x-lifted-pvar-marker id))
                 (extract-ids expanded-form))))
    
  
  (values present-variables lifted-variables))

;(struct splicing-list (l) #:transparent)
(require "cross-phase-splicing-list.rkt")

;; TODO: dotted rest, identifier macro
#;(define-syntax-rule (?@ v ...)
    (splicing-list (list v ...)))
(define (?@ . vs) (splicing-list vs))
(define (?@@ . vs) (splicing-list (map splicing-list vs)))

(define-for-syntax ((?* mode) stx)
  (define (parse stx)
    (syntax-case stx ()
      [(self condition a)
       (?* (datum->syntax stx `(,#'self ,#'c ,#'a ,#'(?@)) stx stx))]
      [(_ condition a b)
       (let ()
         (define/with-syntax (pvar …) (current-pvars-shadowers))

         (define/with-syntax expanded-condition
           (local-expand #'(λ (lifted-variables-hash)
                             (syntax-parameterize ([lift-late-pvars-param
                                                    #'lifted-variables-hash])
                               (detect-present-pvars (pvar …) condition)))
                         'expression
                         '()))

         (define-values (present-variables lifted-variables)
           (extract-present-variables #'expanded-condition stx))

         (define/with-syntax ([lifted-key . lifted-macro+args] …)
           lifted-variables)
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO: lifted stuff!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         (define/with-syntax (test-present-attribute …)
           (for/list ([present? (in-list present-variables)]
                      [pv (in-syntax #'(pvar …))]
                      #:when present?
                      ;; only attributes can have missing elements.
                      #:when (eq? 'attr (car (attribute-info pv '(pvar attr)))))
             #`(attribute* #,pv)))
         
         #`(let ([lifted-list (list (cons 'lifted-key
                                          lifted-macro+args)
                                    …)])
             (if (and test-present-attribute …
                      (andmap cdr lifted-list))
                 #,(if (eq? mode 'if)
                       #'a
                       #'(expanded-condition
                          (make-hash lifted-list)))
                 b)))]))
  (parse stx))

(define-syntax ?if (?* 'if))

(define-syntax (?cond stx)
  (syntax-case stx (else)
    [(self) #'(raise-syntax-error '?cond
                                  "all branches contain omitted elements"
                                  (quote-syntax self))]
    [(self [else]) #'(?@)]
    [(self [else . v]) #'(begin . v)]
    [(self [condition v . vs] . rest)
     (not (free-identifier=? #'condition #'else))
     (let ([otherwise (datum->syntax stx `(,#'self . ,#'rest) stx stx)])
       (datum->syntax stx
                      `(,#'?if ,#'condition ,#'(begin v . vs) ,otherwise)
                      stx
                      stx))]))

(define-syntax (?attr stx)
  (syntax-case stx ()
    [(self condition)
     (datum->syntax stx `(,#'?if ,#'condition #t #f) stx stx)]))

(define-syntax (?? stx)
  (define (parse stx)
    (syntax-case stx ()
      [(self a)
       ((?* 'or) (datum->syntax stx `(,#'self ,#'a ,#'a ,#'(?@)) stx stx))]
      [(self a b)
       ((?* 'or) (datum->syntax stx `(,#'self ,#'a ,#'a ,#'b) stx stx))]
      [(self a b c . rest)
       (let ([else (datum->syntax stx `(,#'self ,#'b ,#'c . ,#'rest) stx stx)])
         (datum->syntax stx `(,#'self ,#'a ,else) stx stx))]))
  (parse stx))

(begin-for-syntax
  (struct presence-info (depth>0? pvar iterated-pvar present? depth) #:prefab))

;;; The body is wrapped in a lambda, with one pvarᵢ for each pvar within scope.
;;; This is used to shadow the pvar with one equal to pvarᵢ, which iterates over
;;; the original pvar. Inside that function, the body is wrapped with
;;; detect-present-pvars, which fully expands the body, leaving a quoted vector
;;; of booleans indicating which pvars are actually used within the body. The
;;; vector is identified by the x-pvar-present-marker scope (created with
;;; make-syntax-introducer), and the extract-present-variables utility finds
;;; that vector in the fully-expanded syntax object.
;;; Auto-generated subscripted pattern variables would normally be derived from
;;; the shadowed pvar. However, this means that within two different ddd forms,
;;; the auto-generated subscripted pvars would be derived from different pvars
;;; (two shadowed copies of the original). This means that the generated pvars
;;; would contain different values. To solve this problem, ddd collaborates with
;;; template-subscripts.rkt. When a subscripted pvar is encountered within a ddd
;;; form, template-subscripts.rkt does not auto-generate its contents.
;;; Instead, it extracts the value of the variable from an additionnal
;;; lifted-variables argument (to the function wrapping the body), and notes down,
;;; marking it with the special scope x-lifted-pvar-marker, so that
;;; extract-present-variables can find it.
;;; In effect, this is semantically equivalent to lifting the problematic
;;; pvar outside of the body.
(define-syntax/case (ddd body . tail) ()
  (define/with-syntax allow-missing?
    (syntax-case #'tail () [() #'#f] [(#:allow-missing) #'#t]))
  (define/with-syntax (pvar …) (current-pvars-shadowers))
  
  (define-temp-ids "~aᵢ" (pvar …))
  (define/with-syntax f
    #`(#%plain-lambda (pvarᵢ … lifted-variables-hash)
                      (shadow pvar pvarᵢ) …
                      (syntax-parameterize ([lift-late-pvars-param
                                             #'lifted-variables-hash])
                        (detect-present-pvars (pvar …)
                                              body))))

  ;; extract all the variable ids present in f
  (define/with-syntax expanded-f (local-expand #'f 'expression '()))

  (define-values (present-variables lifted-variables)
    (extract-present-variables #'expanded-f stx))

  (define/with-syntax ([lifted-key . lifted-macro+args] …) lifted-variables)

  (unless (or (ormap identity present-variables)
              (not (null? lifted-variables)))
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
            (match (attribute-info pv '(pvar attr))
              [(list* _ _valvar depth _)
               (if (> depth 0)
                   (presence-info #t pv pvᵢ #t depth)
                   (presence-info #f pv pvᵢ #t depth))]) ;; TODO: detect shadowed bindings, if the pvar was already iterated on, raise an error (we went too deep).
            (presence-info #f pv pvᵢ #f #f))))
    ;; Pvars which are iterated over
    (define/with-syntax (#s(presence-info _ iterated-pvar iterated-pvarᵢ _ _) …)
      (filter presence-info-depth>0? present?+pvars))

    (when (and (stx-null? #'(iterated-pvar …))
               (null? lifted-variables))
      (no-pvar-to-iterate-error present?+pvars))
    
    ;; If the pvar is iterated, use the iterated pvarᵢ 
    ;; otherwise use the original (attribute* pvar)
    (define/with-syntax (filling-pvar …)
      (map (match-λ [(presence-info #t pv pvᵢ #t _) pvᵢ]
                    [(presence-info #f pv pvᵢ #t _) #`(attribute* #,pv)]
                    [(presence-info #f pv pvᵢ #f _) #'#f])
           present?+pvars)))

  #'(map#f* allow-missing?
            (λ (iterated-pvarᵢ … lifted-key …)
              (expanded-f filling-pvar …
                          (make-hash (list (cons 'lifted-key lifted-key) …))))
            (list (quote-syntax iterated-pvar) …
                  (quote-syntax lifted-key) …) ;; TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! not the real variable
            (list (attribute* iterated-pvar) …
                  lifted-macro+args …)))

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
                    (syntax-e (presence-info-pvar present?+pvar))
                    (presence-info-depth present?+pvar)))
          (filter presence-info-present? present?+pvars))
     "\n   "))))
