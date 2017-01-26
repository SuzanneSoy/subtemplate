#lang racket

(require (for-syntax racket/private/sc)
         rackunit)

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

;; but not if the syntax object is a constant, e.g. #'(1 2 3)
(check-pred all-eq?
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

;; nor it the same syntax object is reuqes
(define stxobj (datum->syntax #'here '(1 2 3))) ;; cached stxobj here
(check-pred all-eq?
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
                 (unless old1
                   ;; Initial set!
                   (set! old1 (hh)))
                 (andmap identity (for/list ([range-b (in-range 5)])
                                    (eq? old1 hh))))))))

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