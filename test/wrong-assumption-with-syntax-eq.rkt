#lang racket
(require (for-syntax racket/private/sc))

(define old #f)

(for/list ([range-a (in-range 100)])
  ;; The contents of the valvar is eq? when using a literal syntax object
  ;; #'(1 2 3), but not with (datum->syntax #'here '(1 2 3)).
  ;; I expected the result to always be different at each execution of the
  ;; with-syntax, but it turns out the syntax object is kept as-is.
  (with-syntax ([(xᵢ ...) #'(1 2 3) #;(datum->syntax #'here '(1 2 3))])
    (define-syntax (hh stx)
      #`#,(syntax-mapping-valvar (syntax-local-value #'xᵢ)))
    (unless old
      (displayln "initial set!")
      (set! old (hh)))
    (andmap identity (for/list ([range-b (in-range 5)])
                       (eq? old hh)))))