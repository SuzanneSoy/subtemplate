#lang racket
(require subtemplate/override
         rackunit)

;; f is defined after xᵢ
(check-equal?
 (let ()
   (define/with-syntax (xᵢ …) #'(a b c))
   (define (f) (list zᵢ ... (syntax->datum (subtemplate (yᵢ …)))))
   (f))
 '(a/z b/z c/z (a/y b/y c/y)))

;; f is defined before xᵢ (still works, yay!)
(check-equal?
 (let ()
   (define (f) (list zᵢ ... (syntax->datum (subtemplate (yᵢ …)))))
   (define/with-syntax (xᵢ …) #'(a b c))
   (f))
 '(a/z b/z c/z (a/y b/y c/y)))