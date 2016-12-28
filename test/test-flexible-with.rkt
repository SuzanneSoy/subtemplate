#lang aful/unhygienic type-expander/lang

(require (lib "phc-graph/flexible-with.hl.rkt")
         (for-syntax racket/syntax
                     racket/list
                     (rename-in racket/base [... …]))
         phc-toolkit
         typed-map)

(define-syntax (gs stx)
  (syntax-case stx ()
    [(_ bt-fields-id nfields (f …) [struct struct-field …] …)
     (let ()
       (define/with-syntax (field …)
         (append (syntax->list #'(f …))
                 (map (λ (_) (datum->syntax #'nfields (gensym 'g)))
                      (range (- (syntax-e #'nfields)
                                (length (syntax->list #'(f …))))))))
       (define-trees #'(bt-fields-id
                        (field …)
                        [struct struct-field …] …)))]))

(gs bt-fields
    16
    (a b c)
    [sab a b]
    [sbc b c]
    [sabc a b c])

(check-equal?:
 (~> (ann (with-c (sab→tree 1 2) 'nine)
          ((bt-fields a b c) One Positive-Byte 'nine))
     force
     flatten
     (filter Some? _)
     (map Some-v _)
     list->set)
 (set 1 2 'nine))
 

(check-equal?:
 (call-with-values
  #λ(tree→sab (sab→tree 1 2))
  list)
 '(1 2))
       
(check-equal?:
 (call-with-values
  #λ(tree→sabc (ann (with-c (sab→tree 1 2) 'nine)
                    ((bt-fields a b c) One Positive-Byte 'nine)))
  list)
 '(1 2 nine))

(check-equal?:
 (call-with-values
  #λ(tree→sabc (with-c (sab→tree 'NONE 'NONE) 'NONE))
  list)
 '(NONE NONE NONE))

(check-equal?:
 (call-with-values
  #λ(tree→sab (without-c (with-c (sab→tree 'NONE 'NONE) 'NONE)))
  list)
 '(NONE NONE))

(check-equal?:
 (call-with-values
  #λ(tree→sbc (without-a (with-c (sab→tree 'NONE 'NONE) 'NONE)))
  list)
 '(NONE NONE))

(check-equal?:
 (call-with-values
  #λ(tree→sbc (without-a (with-c (sab→tree 1 2) 3)))
  list)
 '(2 3))