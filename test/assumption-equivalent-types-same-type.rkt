#lang typed/racket

;; Check that equivalent type specifications are correctly interpreted as
;; being the same type by Typed/Racket.
;; 
;; This was not the case in some situations in older versions of Typed/Racket,
;; but I am not sure whether this reproduces the same issue, or whether this
;; file would typecheck in older versions too.

(let ()
  (define-type (Foo X)
    (U X (List 'foo (Bar X) (Foo X))))

  (define-type (Bar Y)
    (List 'bar (Foo Y)))

  (define-type (Foo2 X)
    (U X (List 'foo (Bar2 X) (Foo2 X))))

  (define-type (Bar2 Y)
    (List 'bar (Foo2 Y)))

  (λ #:∀ (A) ([x : (Foo A)])
    ;; Check here:
    (ann (ann x (Foo2 A)) (Foo A)))
  
  (void))

(struct (a b) st-foo ([a : a] [b : b]))
(struct (a) st-bar ([a : a]))

(let ()
  (define-type (Foo X)
    (U X (st-foo (Bar X) (Foo X))))

  (define-type (Bar Y)
    (st-bar (Foo Y)))

  (define-type (Foo2 X)
    (U X (st-foo (Bar2 X) (Foo2 X))))

  (define-type (Bar2 Y)
    (st-bar (Foo2 Y)))

  (λ #:∀ (A) ([x : (Foo A)])
    ;; Check here:
    (ann (ann x (Foo2 A)) (Foo A)))
  
  (void))