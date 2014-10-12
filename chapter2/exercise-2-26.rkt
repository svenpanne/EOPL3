#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.26

(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
   (root red-blue-subtree?)))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left red-blue-subtree?)
   (right red-blue-subtree?))
  (blue-node
   (subtrees (list-of red-blue-subtree?)))
  (leaf-node
   (num integer?)))

(define mark-leaves-with-red-depth
  (lambda (b)
    (letrec ((loop (lambda (b red-depth)
                     (cases red-blue-subtree b
                       (red-node (left right)
                         (red-node (loop left (+ red-depth 1))
                                   (loop right (+ red-depth 1))))
                       (blue-node (subtrees)
                         (blue-node (map (lambda (s) (loop s red-depth))
                                         subtrees)))
                       (leaf-node (num)
                         (leaf-node red-depth))))))
      (cases red-blue-tree b
        (a-red-blue-tree (root)
          (loop root 0))))))
