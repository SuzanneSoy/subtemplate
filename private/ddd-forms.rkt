#lang racket/base
(provide begin
         define
         let
         (rename-out [begin #%intef-begin])
         (rename-out [app #%app])
         ??
         ?if
         ?cond
         ?attr
         ?@
         ?@@
         splice-append
         splice-append*
         splicing-list?
         splicing-list
         splicing-list-l)

(require racket/list
         subtemplate/private/ddd
         stxparse-info/case
         stxparse-info/parse
         phc-toolkit/untyped
         (prefix-in - (only-in racket/base
                               begin let lambda define))
         (prefix-in - (only-in stxparse-info/case
                               define/with-syntax))
         (for-syntax racket/base
                     racket/list
                     stxparse-info/parse
                     stxparse-info/parse/experimental/template
                     phc-toolkit/untyped)
         (for-meta 2 racket/base)
         (for-meta 2 phc-toolkit/untyped)
         (for-meta 2 stxparse-info/parse))

(begin-for-syntax
  (define (-nest* before after -v -ooo* [depth 0])
    (if (stx-null? -ooo*)
        -v
        (-nest* before
                after
                #`(#,@(syntax->list before) #,-v . #,after)
                (stx-cdr -ooo*)
                (add1 depth))))
  
  (define-syntax nest*
    (syntax-parser
      [(self (before … {~datum %} . after) v ooo*)
       (with-syntax ([s (datum->syntax #'self 'syntax)])
         #'(-nest* (s ((… …) (before …))) (s ((… …) after)) (s v) (s ooo*)))]))

  (define-syntax ddd*
    (syntax-parser
      [(_ e ooo*)
       #'(with-syntax ([dotted (nest* (ddd %) e ooo*)])
           (nest* (append* %)
                  (list dotted)
                  ooo*))]))

  (define-syntax-class ooo
    (pattern {~and ooo {~literal …}}))

  (define-splicing-syntax-class ooo+
    #:attributes (ooo*)
    (pattern {~seq {~and ooo {~literal …}} …+}
             #:with ooo* #'(ooo …)))

  (define-syntax-class not-macro-id
    #:attributes ()
    (pattern id:id
             #:when (not (syntax-local-value #'id (λ () #f))))
    (pattern id:id
             #:when (syntax-pattern-variable?
                     (syntax-local-value #'id (λ () #f)))))

  (define-syntax-class not-macro-expr
    #:attributes ()
    (pattern :not-macro-id)
    (pattern (:not-macro-id . _)))
  
  (define-splicing-syntax-class stmt
    #:literals (define define/with-syntax)
    (pattern {~seq (define name:id e:expr) :ooo+}
             #:with expanded
             #`(-define name
                        #,(nest* (ddd %) e ooo*)))
    (pattern {~seq (define/with-syntax pat e:expr) :ooo+}
             #:with expanded
             #`(-define/with-syntax #,(nest* (% …) pat ooo*)
                                    #,(nest* (ddd %) e ooo*)))
    (pattern {~seq e :ooo+}
             ;#:with expanded #`(apply values #,(ddd* e ooo*))
             #:with expanded (ddd* e ooo*))
    (pattern other
             #:with expanded #'other)))

(define-syntax/parse (begin stmt:stmt …)
  (template (-begin (?@ stmt.expanded) …)))

(define-syntax/parse (let {~optional name:id} ([var . val] …) . body)
  (template (-let (?? name) ([var (begin . val)] …) (begin . body))))

(begin-for-syntax
  (define-splicing-syntax-class arg
    (pattern {~seq e:expr ooo*:ooo+}
             #:with expanded #`(splicing-list #,(ddd* e ooo*)))
    (pattern other
             ;#:with expanded #'(#%app list other)
             #:with expanded #'other))
  (define-syntax-class not-stx-pair
    (pattern () #:with v #''())
    (pattern {~and v {~not (_ . _)}})))
(define-syntax app
  (syntax-parser
    [{~and (_ fn arg:arg … . rest:not-stx-pair)
           {~not (_ _ {~literal …} . _)}} ;; not fn directly followed by a …
     ;#'(#%app apply fn (#%app append arg.expanded …))
     (syntax/top-loc this-syntax
       (#%app apply fn (#%app splice-append arg.expanded … #:rest rest.v)))]
    [(_ arg:arg … . rest:not-stx-pair) ;; shorthand for list creation
     ;#'(#%app apply list (#%app append arg.expanded …))
     (syntax/top-loc this-syntax
       (#%app apply list (#%app splice-append arg.expanded … #:rest rest.v)))]))

(define (splice-append #:rest [rest '()] . l*)
  (splice-append* (if (null? rest) l* (append l* rest))))
(define (splice-append* l*)
  (cond
    [(pair? l*)
     (if (splicing-list? (car l*))
         (splice-append* (append (splicing-list-l (car l*))
                                 (cdr l*)))
         (cons (car l*) (splice-append* (cdr l*))))]
    [(splicing-list? l*)
     (splice-append* (splicing-list-l l*))]
    [else ;; should be null.
     l*]))