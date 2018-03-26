#lang racket

;; We use a weak hash to associate a pvar xᵢ with its the values contained in
;; the derived yᵢ. The assumptions below must hold, otherwise we would risk
;; memory leaks.

(require (for-syntax racket/private/sc)
         rackunit
         version-case)

(define h (make-weak-hasheq))

(define (all-eq? l)
  (foldl (λ (x acc)
           (and (eq? x acc) acc))
         (car l)
         (cdr l)))

;; The data stored in the valvar is unique fore each use of (datum->syntax …)
(check-false
 (check-duplicates
  (for/list ([range-a (in-range 5)])
    (with-syntax ([(xᵢ ...) (datum->syntax #'here '(1 2 3))])
      (define-syntax (hh stx)
        #`(hash-ref! h
                     #,(syntax-mapping-valvar
                        (syntax-local-value #'xᵢ))
                     (gensym)))
      (all-eq? (for/list ([range-b (in-range 5)])
                 (collect-garbage)
                 (collect-garbage)
                 (collect-garbage)
                 (hh)))))))

;; but not if the syntax object is a constant, e.g. #'(1 2 3), in Racket < 6.7
;; I'm not sure how this affects subtemplate in Racket ≥ 6.7, but I suppose it
;; is not a problem, as the beahviour is the same as in the general case where
;; the syntax object is not constant.
(check-pred (version-case
             [(version< (version) "6.90") all-eq?]
             [else (negate all-eq?)])
            (for/list ([range-a (in-range 5)])
              (with-syntax ([(xᵢ ...) #'(1 2 3)]) ;; CHANGED THIS LINE
                (define-syntax (hh stx)
                  #`(hash-ref! h
                               #,(syntax-mapping-valvar
                                  (syntax-local-value #'xᵢ))
                               (gensym)))
                (all-eq? (for/list ([range-b (in-range 5)])
                           (collect-garbage)
                           (collect-garbage)
                           (collect-garbage)
                           (hh))))))

;; nor it the same syntax object is reused, in Racket < 6.7
;; I'm not sure how this affects subtemplate in Racket ≥ 6.7, but I suppose it
;; is not a problem, as the beahviour is the same as in the general case where
;; the syntax object is not shared.
(define stxobj (datum->syntax #'here '(1 2 3))) ;; cached stxobj here
(check-pred (version-case
             [(version< (version) "6.90") all-eq?]
             [else (negate all-eq?)])
            (for/list ([range-a (in-range 5)])
              (with-syntax ([(xᵢ ...) stxobj]) ;; CHANGED THIS LINE
                (define-syntax (hh stx)
                  #`(hash-ref! h
                               #,(syntax-mapping-valvar
                                  (syntax-local-value #'xᵢ))
                               (gensym)))
                (all-eq? (for/list ([range-b (in-range 5)])
                           (collect-garbage)
                           (collect-garbage)
                           (collect-garbage)
                           (hh))))))


;; Another example showing this behaviour:
;; The contents of the valvar is eq? when using a literal syntax object like:
;;     #'(1 2 3)
;; but not with:
;;     (datum->syntax #'here '(1 2 3))
;; I expected the result to always be different at each execution of the
;; with-syntax, but it turns out the syntax object is kept as-is.
;;
;; With racket ≥ 6.7, the syntax object is different, i.e. not eq?, in every
;; invocation of with-syntax.
(begin
  (let ()
    (define old1 #f)

    (check-true
     (andmap identity
             (for/list ([range-a (in-range 100)])
               ;; #'(1 2 3) HERE:
               (with-syntax ([(xᵢ ...) #'(1 2 3)])
                 (define-syntax (hh stx)
                   #`#,(syntax-mapping-valvar (syntax-local-value #'xᵢ)))
                 (if (not old1)
                     ;; Initial set!
                     (set! old1 (hh))
                     (andmap identity (for/list ([range-b (in-range 5)])
                                        ((version-case
                                          [(version< (version) "6.90") eq?]
                                          [else (negate eq?)])
                                         old1
                                         (hh))))))))))

  (let ()
    (define old2 #f)

    (check-equal?
     (let ([res (for/list ([range-a (in-range 100)])
                  ;; CHANGED THIS:
                  (with-syntax ([(xᵢ ...) (datum->syntax #'here '(1 2 3))])
                    (define-syntax (hh stx)
                      #`#,(syntax-mapping-valvar (syntax-local-value #'xᵢ)))
                    (unless old2
                      ;; Initial set!
                      (set! old2 (hh)))
                    (andmap identity (for/list ([range-b (in-range 5)])
                                       (eq? old2 hh)))))])
       (list (car res) (ormap identity (cdr res))))
     '(#t #f))))