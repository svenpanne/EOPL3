#lang eopl
; ------------------------------------------------------------------------------
; Exercise 2.17
; a) As a list with a tag as the first element:
(define var-exp-a (lambda (var) (list 'var var)))
(define lambda-exp-a (lambda (bound-var body) (list 'lambda bound-var body)))
(define app-exp-a (lambda (exp1 exp2) (list 'app exp1 exp2)))

(define var-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'var (car exp)))))
(define lambda-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'lambda (car exp)))))
(define app-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'app (car exp)))))

(define var-exp->var-a (lambda (exp) (cadr exp)))
(define lambda-exp->bound-var-a (lambda (exp) (cadr exp)))
(define lambda-exp->body-a (lambda (exp) (caddr exp)))
(define app-exp->rator-a (lambda (exp) (cadr exp)))
(define app-exp->rand-a (lambda (exp) (caddr exp)))

; b) As symbol/vector/pair:
(define var-exp-b (lambda (var) var))
(define lambda-exp-b (lambda (bound-var body) (vector bound-var body)))
(define app-exp-b (lambda (exp1 exp2) (cons exp1 exp2)))

(define var-exp?-b (lambda (exp) (symbol? exp)))
(define lambda-exp?-b (lambda (exp) (vector? exp)))
(define app-exp?-b (lambda (exp) (pair? exp)))

(define var-exp->var-b (lambda (exp) exp))
(define lambda-exp->bound-var-b (lambda (exp) (vector-ref exp 0)))
(define lambda-exp->body-b (lambda (exp) (vector-ref exp 1)))
(define app-exp->rator-b (lambda (exp) (car exp)))
(define app-exp->rand-b (lambda (exp) (cdr exp)))
