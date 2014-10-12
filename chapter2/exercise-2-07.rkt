#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.7

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((eqv? (car env) 'empty-env)
                            (report-no-binding-found search-var initial-env))
                           ((eqv? (car env) 'extend-env)
                            (let ((saved-var (cadr env))
                                  (saved-val (caddr env))
                                  (saved-env (cadddr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))
