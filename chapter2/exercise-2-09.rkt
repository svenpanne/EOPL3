#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.9

(define has-binding?
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            #f)
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-env (cdr env)))
                              (or (eqv? search-var saved-var) (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'has-binding? "Bad environment ~s" env)))
