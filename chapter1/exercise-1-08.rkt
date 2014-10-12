#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.8

(define drop-including-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (drop-including-first s (cdr los))))))
