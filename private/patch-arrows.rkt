#lang racket

(require (for-template (only-in '#%kernel [module* k:module*])
                       '#%kernel)
         phc-toolkit/untyped
         syntax/parse
         racket/syntax
         racket/list
         racket/contract
         syntax/id-table
         syntax/strip-context
         "fully-expanded-grammar-extract-bindings.rkt")

(provide patch-arrows)


(define/contract (patch-arrows stx)
  (-> syntax? syntax?)
  (define fully-expanded
    ;; TODO: local-expand/capture-lifts is probably not what we want here,
    ;; instead we should just let the lifted expressions pass through.
    (local-expand/capture-lifts stx 'expression (list #'k:module*))
    #;(local-expand stx 'expression (list #'k:module*)))
  (define extracted-list (extract-bindings fully-expanded))
  (define bindings-table (make-immutable-free-id-table (map cons
                                                            extracted-list
                                                            extracted-list)))
  (define patched-acc '())
  
  (define/contract (patch-srcloc id)
    (-> identifier? (or/c #f identifier?))
    (define table-ref (free-id-table-ref bindings-table id #f))
    (if (and table-ref
             ;; all info missing, i.e. (datum->syntax #'lctx 'sym #f) was used
             (not (or (syntax-source id)
                      (syntax-position id)
                      (syntax-line id)
                      (syntax-column id))))
        (datum->syntax id (syntax-e id) table-ref id)
        #f))
  
  (fold-syntax
   (λ (stx rec)
     (define maybe-patched-binders
       (for*/list ([p* (in-value (syntax-property stx 'sub-range-binders))]
                   #:when p*
                   [p (in-list (flatten p*))])
         (match p
           [(vector (? identifier? d) d-start d-len
                    (? identifier? s) s-start s-len)
            (let ([patched-d (patch-srcloc d)]
                  [patched-s (patch-srcloc s)])
              (and (or patched-d patched-s)
                   (vector (or patched-d d) d-start d-len
                           (or patched-s s) s-start s-len)))]
           [(vector (? identifier? d) d-start d-len d-x d-y
                    (? identifier? s) s-start s-len s-x s-y)
            (let ([patched-d (patch-srcloc d)]
                  [patched-s (patch-srcloc s)])
              (and (or patched-d patched-s)
                   (vector (or patched-d d) d-start d-len d-x d-y
                           (or patched-s s) s-start s-len s-x s-y)))]
           [other #| not a sub-range-binder |# #f])))
     (define patched-binders (filter identity maybe-patched-binders))
     (when (not (null? patched-binders))
       (set! patched-acc (cons patched-binders patched-acc)))

     (rec stx))
   fully-expanded)

  (define existing-property (or (syntax-property fully-expanded
                                                 'sub-range-binders)
                                '()))
  (syntax-property fully-expanded
                   'sub-range-binders
                   (cons patched-acc existing-property)))

;Example usage:
#;(module* test racket
    (require phc-toolkit/untyped)
    (require (for-syntax (submod "..")))
    (require (for-syntax phc-toolkit/untyped
                         racket/syntax))
  
    (define-for-syntax saved (box #f))

    (define-syntax/case (foo y) ()
      (with-arrows
       (record-sub-range-binders! (vector #'y
                                          1 1
                                          (datum->syntax #'y
                                                         (unbox saved)
                                                         #f)
                                          1 1))
       (record-disappeared-uses #'y)
       #'(define y 1)))

    (define-syntax/case (bar body) ()
      (set-box! saved 'aa)
      (patch-arrows #'body))


    (bar
     (begin
       'aa
       (let ([aa 1])
         (let ([aa 1])
           ;; The arrow is drawn from bb to the binding of aa above, thanks to
           ;; the fact that the srcloc is #f for the arrow's origin id. The
           ;; patch-arrows function detects that, and substitutes the
           ;; corresponding definition.
           ;;
           ;; Note that it correctly binds to the nearest let, not the outer aa.
           (foo bb)
           aa)))))
