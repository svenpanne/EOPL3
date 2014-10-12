#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.10

(define extend-env*
  (lambda (vars vals env)
    (cond ((and (null? vars) (null? vals))
           env)
          ((and (pair? vars) (pair? vals))
           (extend-env* (cdr vars)
                               (cdr vals)
                               (extend-env (car vars) (car vals) env)))
          ((null? vars)
           (report-too-few-variables))
          (else
           (report-too-few-values)))))

(define report-too-few-variables
  (lambda ()
    (eopl:error 'extend-env* "Too few variables")))

(define report-too-few-values
  (lambda ()
    (eopl:error 'extend-env* "Too few values")))

; See: exercise 2.5
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))
