#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.13

(define make-env cons)
(define apply-env-func car)
(define empty-env?-func cdr)

(define empty-env
  (lambda ()
    (make-env (lambda (search-var)
                (report-no-binding-found search-var))
              (lambda () #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (make-env (lambda (search-var)
                (if (eqv? search-var saved-var)
                    saved-val
                    (apply-env saved-env search-var)))
              (lambda () #f))))

(define apply-env
  (lambda (env search-var)
    ((apply-env-func env) search-var)))

(define empty-env?
  (lambda (env)
    ((empty-env?-func env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
