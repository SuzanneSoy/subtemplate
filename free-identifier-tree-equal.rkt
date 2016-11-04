#lang racket

(require racket/struct)

(provide free-id-tree=?
         free-id-tree-hash-code
         free-id-tree-secondary-hash-code
         
         free-id-tree-table?
         immutable-free-id-tree-table?
         mutable-free-id-tree-table?
         weak-free-id-tree-table?
         make-immutable-free-id-tree-table
         make-mutable-free-id-tree-table
         make-weak-free-id-tree-table)

(define (free-id-tree=? a b)
  (define rec=? free-id-tree=?)
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
                        (struct->list b)))))]
    [(null? a) (null? b)]
    [else (error (format "Unexpected value for free-id-tree=? : ~a"
                         a))]))

(define ((free-id-tree-hash hc) a)
  (define rec-hash (free-id-tree-hash hc))
  (cond
    [(identifier? a) (hc (syntax-e #'a))]
    [(syntax? a) (rec-hash (syntax-e a))]
    [(pair? a) (hc (cons (rec-hash (car a))
                         (rec-hash (cdr a))))]
    [(vector? a) (hc (list->vector (rec-hash (vector->list a))))]
    [(box? a) (hc (box (rec-hash (unbox a))))]
    [(prefab-struct-key a)
     => (λ (a-key)
          (hc (apply make-prefab-struct a-key
                     (rec-hash (struct->list a)))))]
    [else (hc a)]))

(define free-id-tree-hash-code
  (free-id-tree-hash equal-hash-code))
(define free-id-tree-secondary-hash-code
  (free-id-tree-hash equal-secondary-hash-code))

(define-custom-hash-types free-id-tree-table
  #:key? syntax?
  free-id-tree=?
  free-id-tree-hash-code
  free-id-tree-secondary-hash-code)
