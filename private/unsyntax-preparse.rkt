#lang racket/base

(provide template-ddd
         subtemplate-ddd
         quasitemplate-ddd
         quasisubtemplate-ddd)

(require (rename-in stxparse-info/parse/experimental/template
                    [?? stxparse:??]
                    [?@ stxparse:?@])
         subtemplate/private/ddd-forms
         subtemplate/private/template-subscripts
         (only-in racket/base [... …])
         stxparse-info/parse
         stxparse-info/case
         syntax/stx
         racket/list
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     stxparse-info/parse
                     (only-in racket/base [... …])
                     phc-toolkit/untyped))

(define-for-syntax lifted (make-parameter #f))

(define-for-syntax (pre-parse-unsyntax tmpl depth escapes quasi? form)
  ;; TODO: a nested quasisubtemplate should escape an unsyntax!
  (define (ds e)
    ;; TODO: should preserve the shape of the original stx
    ;; (syntax list vs syntax pair)
    (datum->syntax tmpl e tmpl tmpl))
  (define-syntax-class ooo
    (pattern {~and ooo {~literal ...}}))
  (define (recur t) (pre-parse-unsyntax t depth escapes quasi? form))
  (define (stx-length stx) (length (syntax->list stx)))
  (define (lift! e) (set-box! (lifted) (cons e (unbox (lifted)))))
  (syntax-parse tmpl
    #:literals (unsyntax unsyntax-splicing unquote unquote-splicing
                         quasitemplate ?? ?if ?cond ?attr ?@ ?@@)
    [({~and u unsyntax} (unquote e))
     #:when (and (= escapes 0) quasi?)
     ;; full unsyntax with #,,e
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax-splicing} (unquote e))
     #:when (and (= escapes 0) quasi?)
     ;; full unsyntax-splicing with #,@,e
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax} (unquote-splicing e))
     #:when (and (= escapes 0) quasi?)
     ;; full unsyntax-splicing with #,,@e
     (ds `(,(datum->syntax #'here 'unsyntax-splicing #'u) ,#'e))]
    [({~and u unsyntax} e)
     #:when (and (= escapes 0) quasi?)
     ;; ellipsis-preserving unsyntax with #,e
     ;; If we are nested at depth D, this lifts a syntax pattern variable
     ;; definition for (((tmp ...) ...) ...), with D levels of nesting.
     ;; It uses "begin" from subtemplate/private/ddd-forms to generate the
     ;; values for tmp succinctly. The template #'e is evaluated as many times
     ;; as necessary by "begin", each time stepping the variables under
     ;; ellipses.
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       ;; The value returned by e is wrapped in a list via (splice-append e).
       ;; Normally, the list will contain a single element, unless e was a
       ;; splicing list, in which case it may contain multiple elements.
       (lift! #`(begin (define/with-syntax tmp (splice-append e)) . ooo*))
       ;; Finally, tmp is inserted into the template (the current position is
       ;; under D levels of ellipses) using (?@) to destroy the wrapper list.
       ;; This allows #,(?@ 1 2 3) to be equivalent to #,@(list 1 2 3).
       (ds `(,#'stxparse:?@ . ,(datum->syntax #'tmp #'tmp #'e))))]
    [({~and u unsyntax-splicing} e)
     ;; ellipsis-preserving unsyntax-splicing with #,@e
     ;; This works in the same way as the #,e case just above…
     #:when (and (= escapes 0) quasi?)
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       ;; … with the notable difference that splice-append* is used instead of
       ;; splice-append.
       (lift! #`(begin (define/with-syntax tmp (splice-append* e)) . ooo*))
       (ds `(,#'stxparse:?@ . ,(datum->syntax #'tmp #'tmp #'e))))]
    [({~and u {~or unsyntax unsyntax-splicing}} e)
     ;; Undo one level of protection, so that in #`#`#,x the inner #` adds one
     ;; level of escapement, and #, undoes that escapement.
     ;; Normally, escapes > 0 here (or quasi? is #false)
     (ds `(,#'u ,(pre-parse-unsyntax #'e depth (sub1 escapes) quasi? form)))]
    [(quasitemplate t . opts)
     ;; #`#`#,x does not unquote x, because it is nested within two levels of
     ;; quasitemplate. We reproduce this behaviour here.
     (ds `(,#'quasitemplate
           ,(pre-parse-unsyntax #'t depth (add1 escapes) quasi? form)
           . ,#'opts))]
    [({~and self ?if} condition a b)
     ;; Special handling for the (?if condition a b) meta-operator
     (with-syntax ([tmp (generate-temporary #'self)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (?if #,(form (recur #'condition))
                                                    #,(form (recur #'(a)))
                                                    #,(form (recur #'(b)))))
                       . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and self ?cond} [{~and condition {~not {~literal else}}} . v] . rest)
     ;; Special handling for the ?cond meta-operator, when the first case has
     ;; the shape [condition . v], but not [else . v]
     (recur (ds `(,#'?if ,#'condition
                         ,(ds `(,#'?@ . ,#'v))
                         ,(ds `(,#'self . ,#'rest)))))]
    [({~and self ?cond} [{~literal else}] . rest)
     ;; ?cond meta-operator, when the first case has the shape [else]
     #'(stxparse:?@)]
    [({~and self ?cond} [{~literal else} . v] . rest)
     ;; ?cond meta-operator, when the first case has the shape [else . v]
     (recur #'(?@ . v))]
    [({~and self ?@@} . e)
     ;; Special handling for the special (?@@ . e) meta-operator
     (with-syntax ([tmp (generate-temporary #'self)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp
                         (append* (stx-map*syntax->list #,(form #'e))))
                       . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and self ?attr} condition)
     ;; Special handling for the special (?attr a) meta-operator
     (recur (ds `(,#'?if ,#'condition
                         #t
                         #f)))]
    [(:ooo t)
     ;; Ellipsis used to escape part of a template, i.e. (... escaped)
     tmpl] ;; tmpl is fully escaped: do not change anything, pass the ... along
    [({~and self ??} a b c . rest)
     ;; Extended ?? from syntax/parse with three or more cases
     (ds `(,#'stxparse:?? ,(recur #'a)
                          ,(recur (ds `(,#'self ,#'b ,#'c . ,#'rest)))))]
    [(?? a b)
     ;; ?? from syntax/parse with two cases
     (ds `(,#'stxparse:?? ,(recur #'a) ,(recur #'b)))]
    [(?? a)
     ;; ?? from syntax/parse with a single case (implicit (?@) as the else case)
     (ds `(,#'stxparse:?? ,(recur #'a)))]
    [(?@ . args)
     ;; ?@ from syntax/parse
     (ds `(,#'stxparse:?@ . ,(recur #'args)))]
    [({~var mf (static template-metafunction? "template metafunction")} . args)
     ;; template metafunction from stxparse-info/parse (incompatible with
     ;; syntax/parse's metafunctions until PR racket/racket#1591 is merged).
     (ds `(,#'mf . ,(recur #'args)))]
    [(hd :ooo ...+ . tl)
     ;; (hd ... . tl), with one or more ellipses after hd
     (ds `(,(pre-parse-unsyntax #'hd
                                (+ depth (stx-length #'(ooo …)))
                                escapes
                                quasi?
                                form)
           ,@(syntax->list #'(ooo ...))
           . ,(recur #'tl)))]
    [(hd . tl)
     ;; (hd . tl)
     (ds `(,(recur #'hd) . ,(recur #'tl)))]
    [#(t …)
     ;; #(t …)
     (ds (vector->immutable-vector (list->vector (stx-map recur #'(t …)))))]
    ;; other ids, empty list, numbers, strings, chars, …
    [_ tmpl]))

(define (check-single-result result stx form)
  (unless (and (stx-pair? result) (stx-null? (stx-cdr result)))
    (raise-syntax-error form
                        (string-append "the outer ?@ in the template produced"
                                       " more than one syntax object")
                        stx))
  (stx-car result))

(define-for-syntax ((*template-ddd quasi? form) stx)
  (syntax-case stx ()
    [(_ tmpl . opts)
     (parameterize ([lifted (box '())])
       (let ([new-tmpl (pre-parse-unsyntax #'tmpl 0 0 quasi?
                                           (λ (e) #`(#,form #,e . opts)))])
         (if (null? (unbox (lifted)))
             (datum->syntax stx
                            `(,form ,new-tmpl . ,#'opts)
                            stx
                            stx)
             ((λ (~)
                ;(local-require racket/pretty)
                ;(pretty-write (syntax->datum ~))
                ~)
              (quasisyntax/top-loc stx
                (let-values ()
                  #,@(reverse (unbox (lifted)))
                  (define result
                    #,(datum->syntax stx
                                     `(,form (,new-tmpl) . ,#'opts)
                                     stx
                                     stx))
                  (check-single-result result
                                       (quote-syntax #,stx)
                                       'form)))))))]))

(define-syntax quasitemplate-ddd (*template-ddd #t #'quasitemplate))
(define-syntax quasisubtemplate-ddd (*template-ddd #t #'quasisubtemplate))
(define-syntax template-ddd (*template-ddd #f #'template))
(define-syntax subtemplate-ddd (*template-ddd #f #'subtemplate))

(define (stx-map*syntax->list e)
  (let loop ([l (syntax->list e)])
    (cond
      [(null? l) l]
      [(pair? l) (cons (syntax->list (car l)) (loop (cdr l)))]
      ;; Special treatment for the last element of e: it does not need to
      ;; be a list (as long as ?@ is used in tail position).
      [else l])))