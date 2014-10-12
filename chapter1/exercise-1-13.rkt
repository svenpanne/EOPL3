#lang eopl
; ------------------------------------------------------------------------------
; Exercise 1.13

(define subst
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-sexp new old sexp)) slist)))

(define subst-in-sexp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))
