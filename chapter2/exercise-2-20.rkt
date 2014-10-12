#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.20

; See http://www.haskell.org/haskellwiki/Zipper

; See: exercise 2.19
(define leaf (lambda () '()))
(define at-leaf? null?)

(define fork (lambda (n l r) (list n l r)))
(define current-element car)
(define move-to-left cadr)
(define move-to-right caddr)

(define number->bintree (lambda (n) (fork n (leaf) (leaf))))

(define insert-to-left
  (lambda (n t)
    (fork (current-element t)
          (fork n (move-to-left t) (leaf))
          (move-to-right t))))

(define insert-to-right
  (lambda (n t)
    (fork (current-element t)
          (move-to-left t)
          (fork n (leaf) (move-to-right t)))))

; We represent a location in the binary tree as a pair of a tree (the current
; sub-tree) and a context (the steps to reconstruct the initial tree).
(define location cons)
(define location->tree car)
(define location->context cdr)

; A context is a stack of context elements.
(define root-context (lambda () '()))
(define root-context? null?)
(define push-context-element cons)
(define context->top car)
(define context->parent cdr)

; A context element consists of a direction, a value and a tree.
(define context-element (lambda (d n t) (list d n t)))
(define context-element->tag car)
(define context-element->value cadr)
(define context-element->tree caddr)
(define left (lambda (n t) (context-element 'left n t)))
(define right (lambda (n t) (context-element 'right n t)))
(define left? (lambda (e) (eqv? (context-element->tag e) 'left)))

(define number->bintree-loc
  (lambda (n)
    (location (number->bintree n) (root-context))))

(define current-element-loc
  (lambda (l)
    (current-element (location->tree l))))

(define move-up-loc
  (lambda (l)
    (let ((context (location->context l)))
      (if (root-context? context)
          (eopl:error 'move-up "already at root: ~s" l)
          (let ((top (context->top context)))
            (location (if (left? top)
                          (fork (context-element->value top)
                                 (location->tree l)
                                 (context-element->tree top))
                          (fork (context-element->value top)
                                 (context-element->tree top)
                                 (location->tree l)))
                      (context->parent context)))))))

(define move-to-left-loc
  (lambda (l)
    (let ((t (location->tree l)))
      (location (move-to-left t)
                (push-context-element (left (current-element t)
                                            (move-to-right t))
                                      (location->context l))))))

(define move-to-right-loc
  (lambda (l)
    (let ((t (location->tree l)))
      (location (move-to-right t)
                (push-context-element (right (current-element t)
                                             (move-to-left t))
                                      (location->context l))))))

(define at-root?-loc
  (lambda (l)
    (root-context? (location->context l))))

(define at-leaf?-loc
  (lambda (l)
    (at-leaf? (location->tree l))))

(define insert-to-left-loc
  (lambda (n l)
    (location (insert-to-left n (location->tree l))
              (location->context l))))

(define insert-to-right-loc
  (lambda (n l)
    (location (insert-to-right n (location->tree l))
              (location->context l))))
