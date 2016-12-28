#lang aful/unhygienic hyper-literate type-expander/lang

@title[#;#:style #;(with-html5 manual-doc-style)
       #:tag "flexible-with"
       #:tag-prefix "phc-graph/flexible-with"]{Flexible functional
 modification and extension of records}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/flexible-with"))

@chunk[<*>
       (require (for-syntax (rename-in racket/base [... …])
                            syntax/stx
                            racket/syntax
                            racket/list
                            syntax/id-table
                            racket/sequence)
                (for-meta 2 racket/base)
                "flexible-with-utils.hl.rkt")

       (provide (for-syntax define-trees)
                ;; For tests:
                (struct-out Some))
       
       <maybe>
       <tree-type-with-replacement>
       <define-replace-in-tree>
       <define-remove-in-tree>
       <convert-fields>
       <convert-back-fields>
       <τ-tree-with-fields>
       <define-struct↔tree>
       <define-trees>]

@chunk[<maybe>
       (struct (T) Some ([v : T]) #:transparent)
       (define-type (Maybe T) (U (Some T) 'NONE))]

@section{Type of a tree-record, with a hole}

@CHUNK[<tree-type-with-replacement>
       (define-for-syntax (tree-type-with-replacement n last τ*)
         (define-values (next mod) (quotient/remainder n 2))
         (cond [(null? τ*) last]
               [(= mod 0)
                (tree-type-with-replacement next
                                            #`(Pairof #,last #,(car τ*))
                                            (cdr τ*))]
               [else
                (tree-type-with-replacement next
                                            #`(Pairof #,(car τ*) #,last)
                                            (cdr τ*))]))]

@section{Functionally updating a tree-record}

@subsection{Adding and modifying fields}

Since we only deal with functional updates of immutable records, modifying a
field does little more than discarding the old value, and injecting the new
value instead into the new, updated record.

Adding a new field is done using the same exact operation: missing fields are
denoted by a special value, @racket['NONE], while present fields are
represented as instances of the polymorphic struct @racket[(Some T)]. Adding a
new field is therefore as simple as discarding the old @racket['NONE] marker,
and replacing it with the new value, wrapped with @racket[Some]. A field
update would instead discard the old instance of @racket[Some], and replace it
with a new one.

@CHUNK[<make-replace-in-tree-body>
       (if (= i 1)
           replacement-thunk
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
                               ((next-id (λ () right-subtree)
                                         . replacement?))))))
                 #`(λ ()
                     (let ([tree (tree-thunk)])
                       (let ([left-subtree (car tree)]
                             [right-subtree (cdr tree)])
                         (cons ((next-id (λ () left-subtree)
                                         . replacement?))
                               right-subtree)))))))]

@CHUNK[<define-replace-in-tree>
       (define-for-syntax (define-replace-in-tree names τ* i depth)
         (define/with-syntax name (vector-ref names (sub1 i)))
         (define/with-syntax replacement? #'(replacement))
         (define τ*-limited (take τ* depth))
         #`(begin
             (provide name)
             (: name
                (∀ (#,@τ*-limited T)
                   (→ (→ #,(tree-type-with-replacement i #'Any τ*-limited))
                      T
                      (→ #,(tree-type-with-replacement i #'(Some T) τ*-limited)))))
             (define (name tree-thunk replacement)
               #,(let ([replacement-thunk #'(λ () (Some replacement))])
                   <make-replace-in-tree-body>))))]

@subsection{Removing fields}

TODO: it would be better to factor this out, and simply choose whether to wrap
with Some or use 'NONE on the "front-end" side.

@CHUNK[<define-remove-in-tree>
       (define-for-syntax (define-remove-in-tree names τ* i depth)
         (define/with-syntax name (vector-ref names (sub1 i)))
         (define/with-syntax replacement? #'())
         (define τ*-limited (take τ* depth))
         #`(begin
             (provide name)
             (: name
                (∀ (#,@τ*-limited T)
                   (→ (→ #,(tree-type-with-replacement i #'(Some Any) τ*-limited))
                      (→ #,(tree-type-with-replacement i #''NONE τ*-limited)))))
             (define (name tree-thunk)
               #,(let ([replacement-thunk #'(λ () 'NONE)])
                   <make-replace-in-tree-body>))))]

@section{Auxiliary values}

The following sections reuse a few values which are derived from the list of
fields:

@CHUNK[<utils>
       (define all-fields #'(field …))
       (define depth-above (ceiling-log2 (length (syntax->list #'(field …)))))
       (define offset (expt 2 depth-above))
       (define i*-above (range 1 (expt 2 depth-above)))
       (define names (list->vector
                      (append (map (λ (i) (format-id #'here "-with-~a" i))
                                   i*-above)
                              (stx-map (λ (f) (format-id f "with-~a" f))
                                       #'(field …)))))
       (define rm-names (list->vector
                         (append (map (λ (i) (format-id #'here "-without-~a" i))
                                      i*-above)
                                 (stx-map (λ (f) (format-id f "without-~a" f))
                                          #'(field …)))))]

@section{Type of a tree-record}

@CHUNK[<τ-tree-with-fields>
       (define-for-syntax (τ-tree-with-fields struct-fields fields)
         (define/with-syntax (struct-field …) struct-fields)
         (define/with-syntax (field …) fields)
         <utils>
         ;; Like in convert-from-struct
         (define lookup
           (make-free-id-table
            (for/list ([n (in-syntax all-fields)]
                       [i (in-naturals)])
              (cons n (+ i offset)))))
         (define fields+indices
           (sort (stx-map #λ(cons % (free-id-table-ref lookup %))
                          #'(struct-field …))
                 <
                 #:key cdr))
  
         (define up (* offset 2))

         ;; Like in convert-fields, but with Pairof
         (define (f i)
           ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 `(Some ,(caar fields+indices))
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   ''NONE
                   (begin
                     `(Pairof ,(f (* i 2))
                              ,(f (add1 (* i 2))))))))
         (f 1))]

@section{Conversion to and from record-trees}

@CHUNK[<define-struct↔tree>
       (define-for-syntax (define-struct↔tree
                            offset all-fields τ* struct-name fields)
         (define/with-syntax (field …) fields)
         (define/with-syntax fields→tree-name
           (format-id struct-name "~a→tree" struct-name))
         (define/with-syntax tree→fields-name
           (format-id struct-name "tree→~a" struct-name))
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
             (: fields→tree-name (∀ (field …)
                                    (→ field …
                                       (→ #,(τ-tree-with-fields #'(field …)
                                                                all-fields)))))
             (define (fields→tree-name field …)
               (λ ()
                 #,(convert-fields (* offset 2) fields+indices)))

             (: tree→fields-name (∀ (field …)
                                    (→ (→ #,(τ-tree-with-fields #'(field …)
                                                                all-fields))
                                       (Values field …))))
             (define (tree→fields-name tree-thunk)
               (define tree (tree-thunk))
               #,(convert-back-fields (* offset 2) fields+indices))))]

@subsection{Creating a new tree-record}

@CHUNK[<convert-fields>
       (define-for-syntax (convert-fields up fields+indices)
         ;(displayln fields+indices)
         (define (f i)
           ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 `(Some ,(caar fields+indices))
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   ''NONE
                   `(cons ,(f (* i 2))
                          ,(f (add1 (* i 2)))))))
         ;(displayln (syntax->datum #`#,(f 1)))
         (f 1))]


@subsection{Extracting all the fields from a tree-record}

We traverse the tree in preorder, and accumulate definitions naming the
interesting subparts of the trees (those where there are fields).

@CHUNK[<convert-back-fields>
       (define-for-syntax (convert-back-fields up fields+indices)
         (define result '())
         (define definitions '())
         (define (f i t)
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 (begin
                   (set! result (cons #`(Some-v #,t) result))
                   #t)
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   #f
                   (let* ([left-t (string->symbol
                                   (format "subtree-~a" (* i 2)))]
                          [right-t (string->symbol
                                    (format "subtree-~a" (add1 (* i 2))))]
                          [left (f (* i 2) left-t)]
                          [right (f (add1 (* i 2)) right-t)])
                     (cond
                       [(and left right)
                        (set! definitions (cons #`(define #,left-t (car #,t))
                                                definitions))
                        (set! definitions (cons #`(define #,right-t (cdr #,t))
                                                definitions))
                        #t]
                       [left
                        (set! definitions (cons #`(define #,left-t (car #,t))
                                                definitions))
                        #t]
                       [right
                        (set! definitions (cons #`(define #,right-t (cdr #,t))
                                                definitions))
                        #t]
                       [else
                        #f])))))
         (f 1 #'tree)
         #`(begin #,@definitions (values . #,(reverse result))))]

@section{Defining the converters and accessors for each known record type}

@chunk[<define-trees>
       (define-for-syntax (define-trees stx)
         (syntax-case stx ()
           [(bt-fields-id (field …) [struct struct-field …] …)
            (let ()
              <utils>
              (define ∀-types (map #λ(format-id #'here "τ~a" %)
                                   (range (add1 depth-above))))
              (define total-nb-functions (vector-length names))
              <define-trees-result>)]))]

@CHUNK[<define-trees-result>
       #`(begin
           (define-type-expander (bt-fields-id stx)
             (syntax-case stx ()
               [(_ . fs)
                #`(∀ fs (→ #,(τ-tree-with-fields #'fs
                                                 #'(field …))))]))
           #,@(map #λ(define-replace-in-tree names ∀-types % (floor-log2 %))
                   (range 1 (add1 total-nb-functions)))
           #,@(map #λ(define-remove-in-tree rm-names ∀-types % (floor-log2 %))
                   (range 1 (add1 total-nb-functions)))
           #,@(map #λ(define-struct↔tree
                       offset all-fields ∀-types %1 %2)
                   (syntax->list #'(struct …))
                   (syntax->list #'([struct-field …] …))))]

@include-section[(submod "flexible-with-utils.hl.rkt" doc)]