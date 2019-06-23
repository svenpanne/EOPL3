#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.16

; Alternative 1: Recurse via outer list structure.
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadr (car lst)) (car (car lst)))
              (invert (cdr lst))))))

; Alternative 2: Recursion hidden in map.
(define invert-via-map
  (lambda (lst)
    (map (lambda (lst2) (list (cadr lst2) (car lst2)))
         lst)))
