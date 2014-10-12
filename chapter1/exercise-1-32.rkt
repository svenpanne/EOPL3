#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.32

(define double-tree
  (lambda (b)
    (if (leaf? b)
        (leaf (* (contents-of b) 2))
        (interior-node (contents-of b)
                       (double-tree (lson b))
                       (double-tree (rson b))))))

; See: exercise 1.31
(define leaf (lambda (i) i))
(define interior-node (lambda (s i1 i2) (list s i1 i2)))
(define leaf? integer?)
(define lson cadr)
(define rson caddr)
(define contents-of (lambda (b) (if (leaf? b) b (car b))))
