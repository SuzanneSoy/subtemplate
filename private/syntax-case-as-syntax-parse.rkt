#lang racket/base
(provide ~syntax-case ~syntax-case-stat)
(require stxparse-info/parse
         (for-syntax racket/base))
(define-for-syntax (~syntax-case-impl not-stat? stx)
  (with-syntax ([(_ stx1) stx])
    (define (id=? a b) (and (identifier? a)
                            (free-identifier=? a b)))
    (define (ds e [ctx #'stx1])
      (datum->syntax ctx e ctx ctx))
    (define (ds2 sym [locprop #'stx1])
      (datum->syntax #'here sym locprop locprop))
    (define (sc e)
      (datum->syntax #'here `{~syntax-case ,e} e e))
    (define (process-sequence stx2)
      (syntax-case stx2 ()
        [(pat ooo . rest)
         (and (id=? #'ooo (quote-syntax ...)) not-stat?)
         `(,{sc #'pat} ,#'ooo . ,(process-sequence #'rest))]
        [(pat . rest)
         `(,{sc #'pat} . ,(process-sequence #'rest))]
        [()
         stx2]))
    (syntax-case #'stx1 ()
      [underscore (id=? #'underscore #'_)
                  #'underscore]
      [id (identifier? #'id)
          (ds `{,{ds2 '~var #'id} ,#'id})]
      [(ooo stat) (and (id=? #'ooo (quote-syntax ...)) not-stat?)
                  {ds
                   `(,{ds2 '~syntax-case-stat #'ooo}
                     ,#'stat)}]
      [(pat ooo . rest) (and (id=? #'ooo (quote-syntax ...)) not-stat?)
                        (ds `(,{sc #'pat} ,#'ooo . ,{sc #'rest}))]
      [(pat . rest) (ds `(,{sc #'pat} . ,{sc #'rest}))]
      [() #'stx1]
      [#(pat ...)
       (ds (vector->immutable-vector
            (list->vector
             (process-sequence #'(pat ...)))))]
      [#&pat
       (ds (box-immutable (sc #'pat)))]
      [p
       (prefab-struct-key (syntax-e #'p))
       (ds (make-prefab-struct
            (prefab-struct-key (syntax-e #'p))
            (process-sequence
             (cdr (vector->list (struct->vector (syntax-e #'p)))))))]
      [other
       (ds `{,(ds2 '~datum #'other) ,#'other})])))

(define-syntax ~syntax-case
  (pattern-expander (λ (stx) (~syntax-case-impl #t stx))))
(define-syntax ~syntax-case-stat
  (pattern-expander (λ (stx) (~syntax-case-impl #f stx))))
