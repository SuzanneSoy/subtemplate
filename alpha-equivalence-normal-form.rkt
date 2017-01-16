#lang racket
(let ()
  ;; Given an order-independent let
  (let ([x 'b]
        [y 'a])
    (cons x y))

  ;; e.g. represented roughly as:
  (list 'let
        (set (cons 'x ''b) (cons 'y ''a)) ;; bindings
        (list 'cons 'x 'y))               ;; body

  ;; Can we devise an α-equivalence normal form?

  ;; Let's sort the right-hand side of the let bindings, and number them in that
  ;; order. x gets renamed to var1, and y gets renamed to var0, given the order
  ;; ('a, 'b)
  (let ([var0 'a]
        [var1 'b])
    (cons var1 var0))

  ;; The idea roughly amounts to transforming sets into lists by sorting their
  ;; contents, knowing that the sort operation must not depend on unrenamed
  ;; variables. Given a letrec, what can we do?

  (let ([x 'b]
        [y 'a])
    (letrec ([f (λ (v) (f (cons v x)))]
             [g (λ (v) (g (cons v y)))])
      '…))

  ;; In the example above, x and y can be renamed first, giving var1 and var0
  ;; respectively. Then it becomes possible to sort f and g, as they differ
  ;; by their references to var1 and var0 respectively, and these have already
  ;; been assigned a new number.

  (letrec ([f (λ (v) (f v))]
           [g (λ (v) (f v))])
    '…)

  ;; Here, we have no difference in the values, but there is a difference in the
  ;; way these values refer to the bingings: f refers to itself, while g refers to
  ;; f. Topologically sorting that graph would give a cannon order.

  (letrec ([f (λ (v) (g v))]
           [g (λ (v) (f v))])
    '…)

  ;; Here, there is no difference in the values, and swapping them gives a new
  ;; graph isomorphic to the original. Another more complex case follows:

  (letrec ([f (λ (v) (g v))]
           [g (λ (v) (h v))]
           [h (λ (v) (f v))])
    '…)

  ;; In these cases, the order we assign to each variable does not matter, as they
  ;; are strictly symmetric (in the sense that the bound values are at run-time
  ;; observarionally identical).

  ;; What general solution can we find?
  ;; * What if we topo-sort bindings which cannot be distinguished by their values
  ;; * then, within each SCC, if there are some values which are isomorphic to
  ;;   each other, they can be grouped together for the purpose of numbering.
  ;;   this operation can be repeated.
  ;; * By alternating these two steps, do we systematically get a
  ;;   topologically-sorted DAG, where some nodes are a group of nodes which were
  ;;   isomorphic one level down?
  ;;
  ;; Relevant:
  ;; @inproceedings{babai1983canonical,
  ;;   title={Canonical labeling of graphs},
  ;;   author={Babai, L{\'a}szl{\'o} and Luks, Eugene M},
  ;;   booktitle={Proceedings of the fifteenth annual ACM symposium on Theory of computing},
  ;;   pages={171--183},
  ;;   year={1983},
  ;;   organization={ACM}
  ;; }
  (void))