#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.33

(define mark-leaves-with-red-depth
  (lambda (b)
    (letrec ((loop (lambda (b red-depth)
                     (if (leaf? b)
                         red-depth
                         (let ((new-red-depth
                                (+ red-depth
                                   (if (eqv? (contents-of b) 'red) 1 0))))
                           (interior-node (contents-of b)
                                          (loop (lson b) new-red-depth)
                                          (loop (rson b) new-red-depth)))))))
      (loop b 0))))

; See: exercise 1.31
(define leaf (lambda (i) i))
(define interior-node (lambda (s i1 i2) (list s i1 i2)))
(define leaf? integer?)
(define lson cadr)
(define rson caddr)
(define contents-of (lambda (b) (if (leaf? b) b (car b))))
