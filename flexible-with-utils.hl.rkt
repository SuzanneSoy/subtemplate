#lang aful/unhygienic hyper-literate type-expander/lang

@(require scribble-math)

@title[#:style manual-doc-style]{Utility math functions for binary tree
 manipulation}

@(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/flexible-with"))

@defmodule[(lib "phc-graph/flexible-with-utils.hl.rkt")]

@(unless-preexpanding
  (require (for-label (submod ".."))))

@chunk[<*>
       (require (for-syntax racket/base))
       
       (provide (for-syntax to-bits
                            from-bits
                            floor-log2
                            ceiling-log2))
       
       <to-bits>
       <from-bits>
       <floor-log2>
       <ceiling-log2>
                 
       (module* test racket/base
         (require (for-template (submod "..")))
         (require rackunit)
         <test-to-bits>
         <test-from-bits>)]

@defproc[(to-bits [n exact-nonnegative-integer?]) (listof boolean?)]{}

@CHUNK[<to-bits>
       ;     1      =>                     1
       ;  2     3   =>          10                  11
       ;4   5 6   7 =>    100       101       110       111
       ;89 ab cd ef => 1000 1001 1010 1011 1100 1101 1110 1111

       ;     1      =>                     ""
       ;  2     3   =>           0                   1
       ;4   5 6   7 =>     00        01        10        11
       ;89 ab cd ef =>  000  001  010  011  100  101  110  111

       ;     0      =>                     0
       ;  1     2   =>          1                  10
       ;3   4 5   6 =>    11       100       101       110
       ;78 9a bc de => 111  1000 1001 1010 1011 1100 1101 1110


       (define-for-syntax (to-bits n)
         (reverse
          (let loop ([n n])
            (if (= n 0)
                null
                (let-values ([(q r) (quotient/remainder n 2)])
                  (cons (if (= r 1) #t #f) (loop q)))))))]

@chunk[<test-to-bits>
       (check-equal? (to-bits 0) '())
       (check-equal? (to-bits 1) '(#t))
       (check-equal? (to-bits 2) '(#t #f))
       (check-equal? (to-bits 3) '(#t #t))
       (check-equal? (to-bits 4) '(#t #f #f))
       (check-equal? (to-bits 5) '(#t #f #t))
       (check-equal? (to-bits 6) '(#t #t #f))
       (check-equal? (to-bits 7) '(#t #t #t))
       (check-equal? (to-bits 8) '(#t #f #f #f))
       (check-equal? (to-bits 12) '(#t #t #f #f))
       (check-equal? (to-bits 1024) '(#t #f #f #f #f #f #f #f #f #f #f))]

@defproc[(from-bits [n (listof boolean?)]) exact-nonnegative-integer?]{}

@CHUNK[<from-bits>
       (define-for-syntax (from-bits b)
         (foldl (λ (bᵢ acc)
                  (+ (* acc 2) (if bᵢ 1 0)))
                0
                b))]

@chunk[<test-from-bits>
       (check-equal? (from-bits '()) 0)
       (check-equal? (from-bits '(#t)) 1)
       (check-equal? (from-bits '(#t #f)) 2)
       (check-equal? (from-bits '(#t #t)) 3)
       (check-equal? (from-bits '(#t #f #f)) 4)
       (check-equal? (from-bits '(#t #f #t)) 5)
       (check-equal? (from-bits '(#t #t #f)) 6)
       (check-equal? (from-bits '(#t #t #t)) 7)
       (check-equal? (from-bits '(#t #f #f #f)) 8)
       (check-equal? (from-bits '(#t #t #f #f)) 12)
       (check-equal? (from-bits '(#t #f #f #f #f #f #f #f #f #f #f)) 1024)]

@defproc[(floor-log2 [n exact-positive-integer?])
         exact-nonnegative-integer?]{
 Exact computation of @${\lfloor\log_2(n)\rfloor}.
}

@chunk[<floor-log2>
       (define-for-syntax (floor-log2 n)
         (if (<= n 1)
             0
             (add1 (floor-log2 (quotient n 2)))))]

@defproc[(ceiling-log2 [n exact-positive-integer?])
         exact-nonnegative-integer?]{
 Exact computation of @${\lceil\log_2(n)\rceil}.
}

@chunk[<ceiling-log2>
       (define-for-syntax (ceiling-log2 n)
         (floor-log2 (sub1 (* n 2))))]
