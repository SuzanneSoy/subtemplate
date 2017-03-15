#lang racket

(require (rename-in racket/contract
                    [define/contract define/contract/always]))

(provide (except-out (all-from-out racket/contract)
                     define-struct/contract
                     ;define/contract
                     provide/contract
                     invariant-assertion)
         define/contract
         define/contract/always)

(define-syntax-rule (define/contract sig c . rest)
  (define sig . rest))