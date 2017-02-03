#lang racket

(require subtemplate/private/ddd
         subtemplate/private/unsyntax-preparse
         stxparse-info/case
         stxparse-info/parse
         rackunit
         syntax/macro-testing
         (only-in racket/base [... …]))

;; ??

(define (test-??-all v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean}
       {~optional d:keyword})
      (quasitemplate-ddd (?? a b c d))])))

(check-equal? (test-??-all #'(1 x #f #:kw)) '1)
(check-equal? (test-??-all #'(x #f #:kw)) 'x)
(check-equal? (test-??-all #'(#f #:kw)) '#f)
(check-equal? (test-??-all #'(#:kw)) '#:kw)

(check-equal? (test-??-all #'(1)) '1)
(check-equal? (test-??-all #'(x)) 'x)
(check-equal? (test-??-all #'(#f)) '#f)
(check-equal? (test-??-all #'(#:kw)) '#:kw)

;; ?cond

(define (test-?cond v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean}
       {~optional d:keyword})
      (quasitemplate-ddd (?cond [a 10] [b 20] [c 30] [d 40]))])))

(check-equal? (test-?cond #'(1 x #f #:kw)) 10)
(check-equal? (test-?cond #'(x #f #:kw)) 20)
(check-equal? (test-?cond #'(#f #:kw)) 30)
(check-equal? (test-?cond #'(#:kw)) 40)

(check-equal? (test-?cond #'(1)) 10)
(check-equal? (test-?cond #'(x)) 20)
(check-equal? (test-?cond #'(#f)) 30)
(check-equal? (test-?cond #'(#:kw)) 40)

;; ?attr

(define (test-?attr v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean}
       {~optional d:keyword})
      (quasitemplate-ddd ((?attr a) (?attr b) (?attr c) (?attr d)))])))

(check-equal? (test-?attr #'(1 x #f #:kw)) '(#t #t #t #t))
(check-equal? (test-?attr #'(x #f #:kw))   '(#f #t #t #t))
(check-equal? (test-?attr #'(#f #:kw))     '(#f #f #t #t))
(check-equal? (test-?attr #'(#:kw))        '(#f #f #f #t))

(check-equal? (test-?attr #'(1))    '(#t #f #f #f))
(check-equal? (test-?attr #'(x))    '(#f #t #f #f))
(check-equal? (test-?attr #'(#f))   '(#f #f #t #f))
(check-equal? (test-?attr #'(#:kw)) '(#f #f #f #t))

;; ?if

(define (test-?if v)
  (syntax->datum
   (syntax-parse v
     [({~optional a:nat}
       {~optional b:id}
       {~optional c:boolean})
      (quasitemplate-ddd (?if a b c))])))

(check-equal? (test-?if #'(1 x #f)) 'x)
(check-equal? (test-?if #'(x #f))   '#f)
(check-equal? (test-?if #'(#f))     '#f)
(check-exn #rx"attribute contains non-syntax value"
           (λ ()
             (convert-compile-time-error
              (check-equal? (test-?if #'(1 #f)) '#f))))

(check-equal? (syntax->datum
               (syntax-parse #'(1 x)
                 [({~optional a:nat}
                   {~optional b:id}
                   {~optional c:boolean}
                   {~optional d:keyword})
                  (quasitemplate-ddd (?if a (?if b a d) 0))]))
              1)

;; ?@@

(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (x y) (#f))
                 [(a b c)
                  (quasitemplate-ddd ({?@@ a b c}))]))
              '(1 2 3 x y #f))

(check-equal? (syntax->datum
               (syntax-parse #'((1 2 3) (x y) (#f))
                 [whole
                  (quasitemplate-ddd ({?@@ . whole}))]))
              '(1 2 3 x y #f))
