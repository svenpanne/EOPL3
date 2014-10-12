#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.26

(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))
