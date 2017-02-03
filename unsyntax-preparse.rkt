#lang racket/base

(provide quasitemplate-ddd
         quasisubtemplate-ddd)

(require (rename-in stxparse-info/parse/experimental/template
                    [?? stxparse:??]
                    [?@ stxparse:?@])
         subtemplate/ddd-forms
         subtemplate/template-subscripts
         (only-in racket/base [... …])
         stxparse-info/parse
         stxparse-info/case
         syntax/stx
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
    [({~and u unsyntax} (unquote e)) ;; full unquote with #,,
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax-splicing} (unquote e)) ;; full unquote with #,@,
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax} (unquote-splicing e)) ;; full unquote with #,,@
     (ds `(,(datum->syntax #'here 'unsyntax-splicing #'u) ,#'e))]
    [({~and u unsyntax} e)
     #:when (and (= escapes 0) quasi?)
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (splice-append e)) . ooo*))
       (ds `(,#'stxparse:?@ . ,(datum->syntax #'tmp #'tmp #'e))))]
    [({~and u unsyntax-splicing} e)
     #:when (and (= escapes 0) quasi?)
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (splice-append* e)) . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and u {~or unsyntax unsyntax-splicing}} e)
     ;; when escapes ≠ 0 (or quasi? is #false)
     (ds `(,#'u ,(pre-parse-unsyntax e depth (sub1 escapes) quasi? form)))]
    [(quasitemplate t . opts)
     (ds `(,#'quasitemplate
           ,(pre-parse-unsyntax #'t depth (add1 escapes) quasi? form)
           . ,#'opts))]
    [({~and self ?if} condition a b)
     (with-syntax ([tmp (generate-temporary #'self)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (?if #,(form (recur #'condition))
                                                    #,(form (recur #'(a)))
                                                    #,(form (recur #'(b)))))
                       . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and self ?cond} [{~and condition {~not {~literal else}}} . v] . rest)
     (recur (ds `(,#'?if ,#'condition
                         ,(ds `(,#'?@ . ,#'v))
                         ,(ds `(,#'self . ,#'rest)))))]
    [({~and self ?cond} [{~literal else}] . rest)
     #'(stxparse:?@)]
    [({~and self ?cond} [{~literal else} . v] . rest)
     (recur #'(?@ . v))]
    [({~and self ?@@} . e)
     (with-syntax ([tmp (generate-temporary #'self)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp
                         (append* (stx-map*syntax->list #,(form #'e))))
                       . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and self ?attr} condition)
     (recur (ds `(,#'?if ,#'condition
                         #t
                         #f)))]
    [(:ooo t)
     tmpl] ;; fully escaped, do not change
    [({~and self ??} a b c . rest)
     (ds `(,#'stxparse:?? ,(recur #'a)
                          ,(recur (ds `(,#'self ,#'b ,#'c . ,#'rest)))))]
    [(?? a b)
     (ds `(,#'stxparse:?? ,(recur #'a) ,(recur #'b)))]
    [(?? a)
     (ds `(,#'stxparse:?? ,(recur #'a)))]
    [(?@ . args)
     (ds `(,#'stxparse:?@ . ,(recur #'args)))]
    [({~var mf (static template-metafunction? "template metafunction")} . args)
     (ds `(,#'mf . ,(recur #'args)))]
    [(hd :ooo ...+ . tl)
     (ds `(,(pre-parse-unsyntax #'hd
                                (+ depth (stx-length #'(ooo …)))
                                escapes
                                quasi?
                                form)
           ,@(syntax->list #'(ooo ...))
           . ,(recur #'tl)))]
    [(hd . tl)
     (ds `(,(recur #'hd) . ,(recur #'tl)))]
    [#(t …)
     (ds (list->vector (stx-map recur #'(t …))))]
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
                  (check-single-result result (quote-syntax stx) 'form)))))))]))

(define-syntax quasitemplate-ddd (*template-ddd #t #'quasitemplate))
(define-syntax quasisubtemplate-ddd (*template-ddd #t #'quasisubtemplate))
(define-syntax template-ddd (*template-ddd #t #'template))
(define-syntax subtemplate-ddd (*template-ddd #t #'subtemplate))

(define (stx-map*syntax->list e)
  (let loop ([l (syntax->list e)])
    (cond
      [(null? l) l]
      [(pair? l) (cons (syntax->list (car l)) (loop (cdr l)))]
      ;; Special treatment for the last element of e: it does not need to
      ;; be a list (as long as ?@ is used in tail position).
      [else l])))