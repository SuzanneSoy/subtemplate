#lang racket

(require racket/struct
         ;; TODO: move delay-pure/private/immutable-struct to a separate package
         delay-pure/private/immutable-struct) ;; for immutable-struct? below.

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

;; Contract:
;; TODO: move to tr-immutable
(define isyntax/c
  (flat-rec-contract isyntax
                     (or/c boolean?
                           char?
                           number?
                           keyword?
                           null?
                           (and/c string? immutable?)
                           symbol?
                           (box/c isyntax #:immutable #t)
                           (cons/c isyntax isyntax)
                           (vectorof isyntax #:immutable #t)
                           (syntax/c isyntax)
                           (and/c immutable-struct?
                                  prefab-struct-key
                                  (λ (v)
                                    (andmap isyntax/c (struct->list v)))))))

(define/contract (free-id-tree=? a b [r equal?])
  (-> isyntax/c isyntax/c boolean?)
  (define (rec=? a b) (free-id-tree=? a b r))
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
    [else (equal? a b)]))

(define/contract ((free-id-tree-hash hc) a)
  (-> (-> any/c fixnum?) (-> isyntax/c fixnum?))
  (define rec-hash (free-id-tree-hash hc))
  (cond
    [(identifier? a) (hc (syntax-e #'a))]
    [(syntax? a) (rec-hash (syntax-e a))]
    [(pair? a) (hc (cons (rec-hash (car a))
                         (rec-hash (cdr a))))]
    [(vector? a) (hc (list->vector (map rec-hash (vector->list a))))]
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
