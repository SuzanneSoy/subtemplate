#lang racket
;; This module is an experiment to extract the pattern variables defined by a
;; define/with-syntax form (it could easily be made to work with
;; define/syntax-parse too). Ti relies on inspecting current-pvars before and
;; after the define/with-syntax form. In order to be able to access the updated
;; current-pvars, the macro needs to call a second macro which gets expanded
;; after the define/with-syntax.

(require stxparse-info/parse
         stxparse-info/case)
(require stxparse-info/current-pvars
         (for-syntax racket/list))

(define result #f)

(define-syntax (continue stx)
  (syntax-case stx ()
    [(_ old-pvars-stx)
     (let ()
       (define old-pvars (syntax->list #'old-pvars-stx))
       (define now-pvars (current-pvars))
       (define-values (new-pvars rest-pvars)
         (split-at now-pvars (- (length now-pvars) (length old-pvars))))
       (unless (andmap free-identifier=? old-pvars rest-pvars)
         (log-error
          (string-append "Internal error: The tail of current-pvars changed"
                         " between two calls.\n"
                         " Before: ~a\n"
                         " After: ~a\n"
                         " New items: ~a"
                         old-pvars
                         rest-pvars
                         new-pvars)))
       ;; Return the result for tests:
       #`(set! result '#,new-pvars))]))

(define-syntax (find-defined-pvars stx)
  (syntax-case stx ()
    [(_ pat val)
     #`(begin
         (define/with-syntax pat val)
         (continue #,(current-pvars)))]))

(define/with-syntax (a . b) #'(1 2))
(find-defined-pvars (x . y) #'(3 4))
(define/with-syntax (c . d) #'(5 6))

(module+ test
  (require rackunit)
  (check-equal? result '(y x)))