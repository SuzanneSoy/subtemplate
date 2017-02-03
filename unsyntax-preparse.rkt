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
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     stxparse-info/parse
                     (only-in racket/base [... …])
                     phc-toolkit/untyped))

(define-for-syntax lifted (make-parameter #f))

(define-for-syntax (pre-parse-unsyntax tmpl depth escapes)
  ;; TODO: a nested quasisubtemplate should escape an unsyntax!
  (define (ds e)
    ;; TODO: should preserve the shape of the original stx
    ;; (syntax list vs syntax pair)
    (datum->syntax tmpl e tmpl tmpl))
  (define-syntax-class ooo
    (pattern {~and ooo {~literal ...}}))
  (define (recur t) (pre-parse-unsyntax t depth escapes))
  (define (stx-length stx) (length (syntax->list stx)))
  (define (lift! e) (set-box! (lifted) (cons e (unbox (lifted)))))
  (syntax-parse tmpl
    #:literals (unsyntax unsyntax-splicing unquote unquote-splicing
                         quasitemplate ?? ?@)
    [:id tmpl]
    [({~and u unsyntax} (unquote e)) ;; full unquote with #,,
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax-splicing} (unquote e)) ;; full unquote with #,@,
     (ds `(,#'u ,#'e))]
    [({~and u unsyntax} (unquote-splicing e)) ;; full unquote with #,,@
     (ds `(,(datum->syntax #'here 'unsyntax-splicing #'u) ,#'e))]
    [({~and u unsyntax} e)
     #:when (= escapes 0)
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (splice-append e)) . ooo*))
       (ds `(,#'stxparse:?@ . ,(datum->syntax #'tmp #'tmp #'e))))]
    [({~and u unsyntax-splicing} e)
     #:when (= escapes 0)
     (with-syntax ([tmp (generate-temporary #'e)]
                   [ooo* (map (λ (_) (quote-syntax …)) (range depth))])
       (lift! #`(begin (define/with-syntax tmp (splice-append* e)) . ooo*))
       #'(stxparse:?@ . tmp))]
    [({~and u {~or unsyntax unsyntax-splicing}} e)
     ;; when escapes ≠ 0
     (ds `(,#'u ,(pre-parse-unsyntax e depth (sub1 escapes))))]
    [(quasitemplate t . opts)
     (ds `(,#'quasitemplate ,(pre-parse-unsyntax #'t depth (add1 escapes))
                            . ,#'opts))]
    [({~var mf (static template-metafunction? "template metafunction")} . args)
     (ds `(,#'mf . ,(recur #'args)))]
    [(:ooo t)
     tmpl] ;; fully escaped, do not change
    [(?? . args)
     (ds `(,#'stxparse:?? . ,(recur #'args)))]
    [(?@ . args)
     (ds `(,#'stxparse:?@ . ,(recur #'args)))]
    [(hd :ooo ...+ . tl)
     (ds `(,(pre-parse-unsyntax #'hd (+ depth (stx-length #'(ooo …))) escapes)
           ,@(syntax->list #'(ooo ...))
           . ,(recur #'tl)))]
    [(hd . tl)
     (ds `(,(recur #'hd) . ,(recur #'tl)))]
    [#(t …)
     (ds (list->vector (stx-map recur #'(t …))))]
    [()
     tmpl]))

(define-for-syntax ((quasi*template-ddd form) stx)
  (syntax-case stx ()
    [(_ tmpl . opts)
     (parameterize ([lifted (box '())])
       (let ([new-tmpl (pre-parse-unsyntax #'tmpl 0 0)])
         (if (null? (unbox (lifted)))
             (datum->syntax stx
                            `(,form ,new-tmpl . ,#'opts)
                            stx
                            stx)
             (quasisyntax/top-loc stx
               (let-values ()
                 #,@(unbox (lifted))
                 #,(datum->syntax stx
                                  `(,form ,new-tmpl . ,#'opts)
                                  stx
                                  stx))))))]))

(define-syntax quasitemplate-ddd (quasi*template-ddd #'quasitemplate))
(define-syntax quasisubtemplate-ddd (quasi*template-ddd #'quasisubtemplate))
