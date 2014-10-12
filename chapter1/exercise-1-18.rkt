#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.18

(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (sexp) (swap-in-s-exp s1 s2 sexp)) slist)))

(define swap-in-s-exp
  (lambda (s1 s2 sexp)
    (cond ((not (symbol? sexp)) (swapper s1 s2 sexp))
          ((eqv? s1 sexp) s2)
          ((eqv? s2 sexp) s1)
          (else sexp))))
