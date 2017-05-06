#lang racket/base
(require scribble/manual
         (for-template syntax/parse
                       syntax/parse/experimental/template
                       racket/syntax)
         (for-syntax racket/base
                     racket/syntax))
(define-syntax (mk stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([orig: (format-id #'id "orig:~a" #'id)])
       #'(begin
           (define orig: (racket id))
           (provide orig:)))]))
(define-syntax-rule (mk* id ...) (begin (mk id) ...))
     
(mk* syntax-parse syntax-case with-syntax template quasitemplate syntax
     unsyntax quasisyntax ?? ?@ template/loc quasitemplate/loc #%app
     #%top begin let)