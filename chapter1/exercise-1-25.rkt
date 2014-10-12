#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.25

(define exists?
  (lambda (pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) #t)
          (else (exists? pred (cdr lst))))))
