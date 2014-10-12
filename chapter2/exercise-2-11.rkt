#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.11

(define empty-env (lambda () '()))

(define extend-env
  (lambda (var val env)
    (extend-env* (list var) (list val) env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((var-index (lambda (vars)
                          (list-index (lambda (v) (eqv? search-var v)) vars)))
             (loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((index (var-index (caar env))))
                              (if (number? index)
                                  (list-ref (cdar env) index)
                                  (loop (cdr env)))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define list-index
  (lambda (pred lst)
    (letrec ((loop (lambda (lst n)
                     (cond ((null? lst) #f)
                           ((pred (car lst)) n)
                           (else (loop (cdr lst) (+ n 1)))))))
      (loop lst 0))))
             
(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))
