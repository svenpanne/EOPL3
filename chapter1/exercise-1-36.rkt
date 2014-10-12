#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.36
(define g
  (lambda (num-and-sexp lst)
    (cons num-and-sexp
          (map (lambda (ns) (cons (+ (car ns) 1) (cdr ns))) lst))))

(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))
