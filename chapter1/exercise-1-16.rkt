#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.16

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadr (car lst)) (car (car lst)))
              (invert (cdr lst))))))
