#lang racket

(require racket/contract)

(provide (except-out (all-from-out racket/contract)
                     define-struct/contract
                     ;define/contract
                     provide/contract
                     invariant-assertion)
         define/contract)

(define-syntax-rule (define/contract sig c . rest)
  (define sig . rest))