#lang racket
(require (for-syntax racket/syntax))
;; x is first bound with a let inside the local-expanded code.
;; The identifier is extracted (presumably with that let's scope,
;; and re-uesd as a definition outside of the let.
;; Check that this is okay (no "ambiguous identifier" or "identifier
;; used out of context" error.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ e)
     (let ()
       (define/with-syntax whole
         (local-expand #'(let-values ([(e) 2]) e) 'expression '()))
       (define/with-syntax (_ _ xx) #'whole)
       #'(let-values ()
           (define xx 3)
           (list xx
                 whole)))]))

(let ([x 1])
  (test x))

(define-syntax (test2 stx)
  (syntax-case stx ()
    [(_ e)
     (let ()
       (define/with-syntax whole
         (local-expand #'(let-values ([(e) 2]) e) 'expression '()))
       (define/with-syntax (_ _ xx) #'whole)
       #'(let-values ([(xx) xx])
           (list xx
                 whole)))]))

;; This does produce an error. The xxx must not be used as an expression.
#;(let ([x 1])
    (test2 x))