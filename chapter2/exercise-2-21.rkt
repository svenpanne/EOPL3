#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.21

(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var var?)
   (saved-val scheme-value?)
   (saved-env env?)))

(define var? symbol?)
(define scheme-value? (lambda (s) #t))

(define apply-env
  (lambda (environment search-var)
    (cases env environment
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define has-binding?
  (lambda (environment search-var)
    (cases env environment
      (empty-env ()
                 #f)
      (extend-env (saved-var saved-val saved-env)
                  (or (eqv? search-var saved-var)
                      (has-binding? saved-env search-var))))))
