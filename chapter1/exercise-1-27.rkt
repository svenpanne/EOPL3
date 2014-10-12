#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.27

(define flatten
  (lambda (slist)
    (cond ((null? slist) '())
          ((symbol? (car slist)) (cons (car slist) (flatten (cdr slist))))
          (else (append (flatten (car slist)) (flatten (cdr slist)))))))

(define flatten-without-append
  (lambda (slist)
    (letrec ((flatten-onto
              (lambda (slist tail)
                (cond ((null? slist) tail)
                      ((symbol? (car slist))
                       (cons (car slist) (flatten-onto (cdr slist) tail)))
                      (else
                       (flatten-onto (car slist)
                                     (flatten-onto (cdr slist) tail)))))))
      (flatten-onto slist '()))))
