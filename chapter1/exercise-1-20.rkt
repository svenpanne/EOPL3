#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.20

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-in-s-exp s (car slist))
           (count-occurrences s (cdr slist))))))

(define count-occurrences-in-s-exp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp))))
