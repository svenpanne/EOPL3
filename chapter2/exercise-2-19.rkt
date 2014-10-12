#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.19

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
