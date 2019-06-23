#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.15

(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))
