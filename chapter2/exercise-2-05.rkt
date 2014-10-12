#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.5

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
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
