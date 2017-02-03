(module cross-phase-splicing-list '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%provide struct:splicing-list
             splicing-list
             splicing-list?
             splicing-list-l)
  (define-values (struct:splicing-list
                  splicing-list
                  splicing-list?
                  splicing-list-ref
                  _splicing-list-set!)
    (#%app make-struct-type
           'splicing-list   ;; name
           #f               ;; super
           1                ;; fields
           0                ;; auto fields
           #f               ;; auto value
           '()              ;; props
           #f               ;; inspector
           #f               ;; proc-spec
           (cons 0 '())     ;; immutables
           #f               ;; guard
           'splicing-list)) ;; constructor-name
  (define-values (splicing-list-l)
    (lambda (instance)
      (splicing-list-ref instance 0))))