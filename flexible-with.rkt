#lang aful/unhygienic hyper-literate type-expander/lang

@chunk[<*>
       (require (for-syntax (rename-in racket/base [... …])
                            syntax/stx
                            racket/syntax
                            racket/list
                            syntax/id-table
                            racket/sequence)
                (for-meta 2 racket/base)
                "flexible-with-utils.rkt")
       
       <→τ>
       <define-replace-in-tree>
       <convert-fields>
       <τ-with-fields>
       <convert-from-struct>
       <mk>
       <utils>
       <example>]

@CHUNK[<→τ>
       (define-for-syntax (→τ n last τ*)
         (define-values (next mod) (quotient/remainder n 2))
         (cond [(null? τ*) last]
               [(= mod 0) (→τ next #`(Pairof #,last #,(car τ*)) (cdr τ*))]
               [else (→τ next #`(Pairof #,(car τ*) #,last) (cdr τ*))]))]

@CHUNK[<make-replace-in-tree-body>
       (if (= i 1)
           #'(λ () replacement)
           (let* ([bits (to-bits i)]
                  [next (from-bits (cons #t (cddr bits)))]
                  [mod (cadr bits)])
             (define/with-syntax next-id (vector-ref names (sub1 next)))
             (if mod
                 #`(λ ()
                     (let ([tree (tree-thunk)])
                       (let ([left-subtree (car tree)]
                             [right-subtree (cdr tree)])
                         (cons left-subtree
                               ((next-id (λ () right-subtree) replacement))))))
                 #`(λ ()
                     (let ([tree (tree-thunk)])
                       (let ([left-subtree (car tree)]
                             [right-subtree (cdr tree)])
                         (cons ((next-id (λ () left-subtree) replacement))
                               right-subtree)))))))]

@CHUNK[<define-replace-in-tree>
       (define-for-syntax (define-replace-in-tree names τ* i depth)
         (define/with-syntax name (vector-ref names (sub1 i)))
         (define τ*-limited (take τ* depth))
         #`(begin
             (provide name)
             (: name
                (∀ (#,@τ*-limited T)
                   (→ (→ #,(→τ i #'Any τ*-limited))
                      T
                      (→ #,(→τ i #'T τ*-limited)))))
             (define (name tree-thunk replacement)
               #,<make-replace-in-tree-body>)))]

@CHUNK[<convert-fields>
       (define-for-syntax (convert-fields up fields+indices)
         ;(displayln fields+indices)
         (define (f i)
           ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 (caar fields+indices)
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   ''MISSING
                   (begin
                     `(cons ,(f (* i 2))
                            ,(f (add1 (* i 2))))))))
         ;(displayln (syntax->datum #`#,(f 1)))
         (f 1))]

@CHUNK[<τ-with-fields>
       (define-for-syntax (τ-tree-with-fields fields all-fields)
         (define/with-syntax (fl …) fields)
         (define/with-syntax (field …) all-fields)
         (let-values ([(all-fields depth-above offset i*-above names τ*)
                       (utils #'(field …))])
           ;; Like in convert-from-struct
           (define lookup
             (make-free-id-table
              (for/list ([n (in-syntax all-fields)]
                         [i (in-naturals)])
                (cons n (+ i offset)))))
           (define fields+indices
             (sort (stx-map #λ(cons % (free-id-table-ref lookup %))
                            #'(fl …))
                   <
                   #:key cdr))
  
           (define up (* offset 2))

           ;; Like in convert-fields, but with Pairof
           (define (f i)
             ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
             (if (and (pair? fields+indices) (= i (cdar fields+indices)))
                 (begin0
                   (caar fields+indices)
                   (set! fields+indices (cdr fields+indices)))
                 (if (>= (* i 2) up) ;; DEPTH
                     ''MISSING
                     (begin
                       `(Pairof ,(f (* i 2))
                                ,(f (add1 (* i 2))))))))
           (f 1)))]

@CHUNK[<convert-from-struct>
       (define-for-syntax (convert-from-struct
                           offset all-fields τ* struct-name fields)
         (define/with-syntax (field …) fields)
         (define/with-syntax conv-name
           (format-id struct-name "convert-~a" struct-name))
         (define lookup
           (make-free-id-table
            (for/list ([n (in-syntax all-fields)]
                       [i (in-naturals)])
              (cons n (+ i offset)))))
         (define fields+indices
           (sort (stx-map #λ(cons % (free-id-table-ref lookup %))
                          fields)
                 <
                 #:key cdr))
         #`(begin
             (: conv-name (∀ (field …)
                             (→ field …
                                (→ #,(τ-tree-with-fields #'(field …)
                                                         all-fields)))))
             (define (conv-name field …)
               (λ ()
                 #,(convert-fields (* offset 2) fields+indices)))))]

@CHUNK[<mk>
       (define-for-syntax (mk stx)
         (syntax-case stx ()
           [(bt-fields-id (field …) [struct struct-field …] …)
            (let-values ([(all-fields depth-above offset i*-above names τ*)
                          (utils #'(field …))])
              (define total-nb-functions (vector-length names))
              #`(begin
                  (define-type-expander (bt-fields-id stx)
                    (syntax-case stx ()
                      [(_ . fs)
                       #`(∀ fs (→ #,(τ-tree-with-fields #'fs
                                                        #'(field …))))]))
                  #,@(map #λ(define-replace-in-tree names τ* % (floor-log2 %))
                          (range 1 (add1 total-nb-functions)))
                  #,@(map #λ(convert-from-struct
                             offset all-fields τ* %1 %2)
                          (syntax->list #'(struct …))
                          (syntax->list #'([struct-field …] …)))))]))]

@CHUNK[<utils>
       (define-for-syntax (utils stx)
         (syntax-case stx ()
           [(field …)
            (let* ([all-fields #'(field …)]
                   [depth-above (ceiling-log2 (length (syntax->list #'(field …))))]
                   [offset (expt 2 depth-above)]
                   [i*-above (range 1 (expt 2 depth-above))]
                   [names (list->vector
                           (append (map (λ (i) (format-id #'here "-with-~a" i))
                                        i*-above)
                                   (stx-map (λ (f) (format-id f "with-~a" f))
                                            #'(field …))))]
                   [τ* (map #λ(format-id #'here "τ~a" %) (range (add1 depth-above)))])
              (values all-fields
                      depth-above
                      offset
                      i*-above
                      names
                      τ*))]))]
@CHUNK[<example>
       (define-syntax (gs stx)
         (syntax-case stx ()
           [(_ bt-fields-id nfields (f …) [struct struct-field …] …)
            (let ()
              (define/with-syntax (field …)
                (append (syntax->list #'(f …))
                        (map (λ (_) (datum->syntax #'nfields (gensym 'g)))
                             (range (- (syntax-e #'nfields)
                                       (length (syntax->list #'(f …))))))))
              (mk #'(bt-fields-id (field …) [struct struct-field …] …)))]))

       ;(gs 6)
       (gs bt-fields
           16
           (a b c)
           [sab a b]
           [sbc b c])

       (ann (with-c (convert-sab 1 2) 'nine)
            ((bt-fields a b c) One Positive-Byte 'nine))]