#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.21

(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (map (lambda (s2) (list (car sos1) s2)) sos2)
                (product (cdr sos1) sos2)))))
