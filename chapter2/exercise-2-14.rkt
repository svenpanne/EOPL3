#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.14

(define make-env
  (lambda (apply-env-func2 empty-env-func2 has-binding?-func2)
    (list apply-env-func2 empty-env-func2 has-binding?-func2)))

(define apply-env-func car)
(define empty-env?-func cadr)
(define has-binding?-func caddr)

(define empty-env
  (lambda ()
    (make-env (lambda (search-var)
                 (report-no-binding-found search-var))
               (lambda () #t)
               (lambda (search-var) #f))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (make-env (lambda (search-var)
                 (if (eqv? search-var saved-var)
                     saved-val
                     (apply-env saved-env search-var)))
               (lambda () #f)
               (lambda (search-var)
                 (or (eqv? search-var saved-var)
                     (has-binding? saved-env search-var))))))

(define apply-env
  (lambda (env search-var)
    ((apply-env-func env) search-var)))

(define empty-env?
  (lambda (env)
    ((empty-env?-func env))))

(define has-binding?
  (lambda (env search-var)
    ((has-binding?-func env) search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
