#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.23

(define list-index
  (lambda (pred lst)
    (letrec ((loop (lambda (lst n)
                     (cond ((null? lst) #f)
                           ((pred (car lst)) n)
                           (else (loop (cdr lst) (+ n 1)))))))
      (loop lst 0))))
