#lang racket

(require racket/struct)

(provide free-identifier-tree=?)

(define (free-identifier-tree=? a b)
  (define rec=? free-identifier-tree=?)
  (cond
    [(identifier? a) (and (identifier? b)
                          (free-identifier=? a b))]
    [(syntax? a) (and (syntax? b)
                      (rec=? (syntax-e a)
                             (syntax-e b)))]
    [(pair? a) (and (pair? b)
                    (rec=? (car a) (car b))
                    (rec=? (cdr a) (cdr b)))]
    [(vector? a) (and (vector? b)
                      (rec=? (vector->list a)
                             (vector->list b)))]
    [(box? a) (and (box? b)
                   (rec=? (unbox a)
                          (unbox b)))]
    [(prefab-struct-key a)
     => (λ (a-key)
          (let ([b-key (prefab-struct-key b)])
            (and (equal? a-key b-key)
                 (rec=? (struct->list a)
                        (struct->list b)))))]))