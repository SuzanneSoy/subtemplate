#lang racket

(provide (struct-out graph-info)
         (struct-out node-info)
         (struct-out field-info)
         (struct-out rich-graph-info)
         (struct-out rich-mapping-info))

(struct graph-info (name
                    multi-constructor
                    root
                    node-order
                    nodes))
(struct node-info (constructor
                   predicate?
                   field-order
                   fields
                   promise-type
                   make-incomplete-type  ;; may be removed later
                   incomplete-type
                   make-placeholder-type ;; may be removed later.
                   placeholder-type))
(struct field-info (type
                    incomplete-type))

(struct rich-graph-info (name
                         multi-constructor
                         root-node
                         root-mapping
                         node-order
                         nodes
                         mapping-order
                         mappings))
(struct rich-mapping-info (constructor
                           ;predicate?
                           ;field-order
                           ;fields
                           with-promises-type
                           ;make-incomplete-type  ;; may be removed later
                           ;incomplete-type
                           ;make-placeholder-type ;; may be removed later.
                           ;placeholder-type
                           ))