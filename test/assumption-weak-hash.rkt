#lang racket

(require (for-syntax racket/private/sc))

(define h (make-weak-hasheq))

(define (all-eq? l)
  (foldl (λ (x acc)
           (and (eq? x acc) acc))
         (car l)
         (cdr l)))

(for/list ([range-a (in-range 100)])
  (with-syntax ([(xᵢ ...) #'(1 2 3)])
    (define-syntax (hh stx)
      #`(hash-ref! h
                   #,(syntax-mapping-valvar (syntax-local-value #'xᵢ))
                   (gensym)))
    (displayln (hash->list h))
    (all-eq? (for/list ([range-b (in-range 5)])
               ;(collect-garbage)
               ;(collect-garbage)
               ;(collect-garbage)
               (hh)))))