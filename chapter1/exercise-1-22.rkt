#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.22

(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst))))))
