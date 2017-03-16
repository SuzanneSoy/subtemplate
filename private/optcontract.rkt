#lang racket

(provide (except-out (all-from-out racket/contract)
                     define-struct/contract
                     ;define/contract
                     provide/contract
                     invariant-assertion)
         define/contract
         define/contract/always
         define/contract/alt)

(require (rename-in racket/contract
                    [define/contract define/contract/always]))

(define-syntax-rule (define/contract sig c . rest)
  (define sig . rest))

;; The alt-code is executed in the body of the function when the contract is
;; disabled:
(define-syntax-rule (define/contract/alt sig c alt-code . rest)
  (define sig alt-code . rest))