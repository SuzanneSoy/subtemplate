#lang racket
(require rackunit)
(define-for-syntax outer #f)
(define-for-syntax inner #f)

(check-equal? (let ([x 1])
                (define-syntax (capture1 stx)
                  (set! outer #'x)
                  #'(void))
                (capture1)
                (let ([x 2])
                  (define-syntax (capture2 stx)
                    (set! inner #'x)
                    #'(void))
                  (capture2)
                  (let ([y 3])
                    (define-syntax (compare stx)
                      (define candidate (datum->syntax #'y 'x))
                      ;; check that (datum->syntax #'y 'x) matches the
                      ;; inner x, but not the outer x, since they are already
                      ;; bound when the macro is executed.
                      #`(list #,(free-identifier=? candidate inner)
                              #,(free-identifier=? candidate outer)))
                    (compare))))
              '(#t #f))