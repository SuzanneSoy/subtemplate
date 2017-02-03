#lang racket/base
(require (only-in subtemplate/ddd-forms
                  splicing-list
                  splice-append
                  splice-append*)
         rackunit)

(define (mk . vs) (splicing-list vs))

(check-equal? (splice-append* '(1 2 3)) '(1 2 3))
(check-equal? (splice-append* (mk 1 2 3)) '(1 2 3))
(check-equal? (splice-append* (mk (mk 1 2 3))) '(1 2 3))
(check-equal? (splice-append* (mk (mk (mk 1 2 3)))) '(1 2 3))
(check-equal? (splice-append* (mk -1 (mk 0 (mk 1 2 3) 4 5) 6 7))
              '(-1 0 1 2 3 4 5 6 7))

(check-equal? (splice-append '(1 2 3)) '((1 2 3)))
(check-equal? (splice-append (mk 1 2 3)) '(1 2 3))
(check-equal? (splice-append (mk (mk 1 2 3))) '(1 2 3))
(check-equal? (splice-append (mk (mk (mk 1 2 3)))) '(1 2 3))
(check-equal? (splice-append (mk -1 (mk 0 (mk 1 2 3) 4 5) 6 7))
              '(-1 0 1 2 3 4 5 6 7))